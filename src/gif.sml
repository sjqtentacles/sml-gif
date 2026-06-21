(* gif.sml -- pure-SML animated GIF89a encoder.

   Pipeline:
     1. scan every frame to collect distinct RGB colors (alpha ignored);
     2. build a global color table -- exact (lossless) when a sequence has
        <= 256 distinct colors, otherwise median-cut quantization to 256;
     3. map each pixel to a palette index (nearest color, memoized);
     4. GIF-variant LZW compress the indices (variable-width codes, LSB-first,
        clear/EOI codes, dictionary reset at 4096);
     5. emit GIF89a: header, logical screen descriptor, global color table,
        NETSCAPE2.0 loop extension, then per frame a Graphic Control Extension,
        Image Descriptor and the LZW sub-blocks; finally the trailer byte.

   The LZW code-width stepping matches the giflib/spec convention, so output is
   decodable by standard viewers. Everything is integer arithmetic over the
   Basis library only: total and deterministic, byte-identical across MLton and
   Poly/ML. *)

structure Gif :> GIF =
struct
  exception Gif of string

  type frame = { image : Image.image, delayCs : int }

  (* ---------- small bit helpers ---------- *)

  fun pow2 k = Word.toInt (Word.<< (0w1, Word.fromInt k))

  (* ---------- growable byte buffer ---------- *)

  type bbuf = { data : Word8Array.array ref, len : int ref }

  fun newBuf () : bbuf = { data = ref (Word8Array.array (256, 0w0)), len = ref 0 }

  fun reserve ({ data, len } : bbuf) extra =
    let
      val cap = Word8Array.length (!data)
      val need = !len + extra
    in
      if need <= cap then ()
      else
        let
          fun grow c = if c >= need then c else grow (c * 2)
          val ncap = grow (cap * 2)
          val na = Word8Array.array (ncap, 0w0)
        in
          Word8Array.copy { src = !data, dst = na, di = 0 };
          data := na
        end
    end

  fun pushByte (b as { data, len } : bbuf) (w : Word8.word) =
    ( reserve b 1
    ; Word8Array.update (!data, !len, w)
    ; len := !len + 1 )

  fun pushInt b i = pushByte b (Word8.fromInt (i mod 256))
  fun pushU16 b n = (pushInt b (n mod 256); pushInt b (n div 256 mod 256))
  fun pushString b s = CharVector.app (fn c => pushInt b (Char.ord c)) s

  fun bufToVector ({ data, len } : bbuf) =
    Word8ArraySlice.vector (Word8ArraySlice.slice (!data, 0, SOME (!len)))

  (* ---------- growable open-addressing int->int map ---------- *)
  (* keys are non-negative; empty slots hold ~1. Used for the distinct-color
     set and the color->index memo. *)

  type imap = { keys : int array ref, vals : int array ref
              , size : int ref, cap : int ref }

  fun newMap () : imap =
    { keys = ref (Array.array (1024, ~1)), vals = ref (Array.array (1024, 0))
    , size = ref 0, cap = ref 1024 }

  fun mapSlot (keys, cap, key) =
    let
      fun probe h =
        let val k = Array.sub (keys, h)
        in if k = ~1 orelse k = key then h else probe ((h + 1) mod cap) end
    in
      probe (key mod cap)
    end

  fun mapFind ({ keys, vals, cap, ... } : imap) key =
    let val h = mapSlot (!keys, !cap, key)
    in if Array.sub (!keys, h) = key then SOME (Array.sub (!vals, h)) else NONE end

  fun mapGrow (m as { keys, vals, size, cap } : imap) =
    let
      val oldKeys = !keys and oldVals = !vals and oldCap = !cap
      val ncap = oldCap * 2
      val nk = Array.array (ncap, ~1)
      val nv = Array.array (ncap, 0)
      fun reinsert i =
        if i >= oldCap then ()
        else
          let val k = Array.sub (oldKeys, i)
          in
            if k <> ~1 then
              let val h = mapSlot (nk, ncap, k)
              in Array.update (nk, h, k); Array.update (nv, h, Array.sub (oldVals, i)) end
            else ();
            reinsert (i + 1)
          end
    in
      reinsert 0; keys := nk; vals := nv; cap := ncap
    end

  (* insert if absent; returns true if newly inserted *)
  fun mapInsert (m as { keys, vals, size, cap } : imap) (key, v) =
    let val h = mapSlot (!keys, !cap, key)
    in
      if Array.sub (!keys, h) = key then false
      else
        ( Array.update (!keys, h, key)
        ; Array.update (!vals, h, v)
        ; size := !size + 1
        ; if !size * 10 >= !cap * 7 then mapGrow m else ()
        ; true )
    end

  fun mapPut (m as { keys, vals, cap, ... } : imap) (key, v) =
    let val h = mapSlot (!keys, !cap, key)
    in
      if Array.sub (!keys, h) = key then Array.update (!vals, h, v)
      else ignore (mapInsert m (key, v))
    end

  (* ---------- growable int buffer (distinct colors) ---------- *)

  type ibuf = { data : int array ref, len : int ref }
  fun newIBuf () : ibuf = { data = ref (Array.array (256, 0)), len = ref 0 }
  fun ipush ({ data, len } : ibuf) (x : int) =
    let val cap = Array.length (!data)
    in
      if !len < cap then ()
      else
        let val na = Array.array (cap * 2, 0)
        in Array.copy { src = !data, dst = na, di = 0 }; data := na end;
      Array.update (!data, !len, x);
      len := !len + 1
    end

  (* ---------- list merge sort ---------- *)

  fun msort cmp xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmp (x, y) of GREATER => y :: merge (x :: xs, ys)
                              | _ => x :: merge (xs, y :: ys))
      fun split [] = ([], [])
        | split [x] = ([x], [])
        | split (x :: y :: rest) =
            let val (a, b) = split rest in (x :: a, y :: b) end
      fun go [] = []
        | go [x] = [x]
        | go xs =
            let val (a, b) = split xs in merge (go a, go b) end
    in
      go xs
    end

  (* ---------- color helpers ---------- *)

  fun rgbKey (r, g, b) = r * 65536 + g * 256 + b
  fun keyR k = k div 65536
  fun keyG k = (k div 256) mod 256
  fun keyB k = k mod 256

  (* ---------- median-cut quantization over distinct colors ---------- *)

  fun medianCut (colors : (int * int * int) list, maxColors) =
    let
      val arr = Array.fromList colors
      val n = Array.length arr
      fun chan 0 (r, _, _) = r
        | chan 1 (_, g, _) = g
        | chan _ (_, _, b) = b
      fun rangeOf (lo, hi) =
        let
          fun ext c =
            let
              fun lp i (mn, mx) =
                if i > hi then (mn, mx)
                else
                  let val v = chan c (Array.sub (arr, i))
                  in lp (i + 1) (Int.min (mn, v), Int.max (mx, v)) end
              val (mn, mx) = lp lo (255, 0)
            in mx - mn end
          val sr = ext 0 and sg = ext 1 and sb = ext 2
        in
          if sr >= sg andalso sr >= sb then (0, sr)
          else if sg >= sb then (1, sg)
          else (2, sb)
        end
      fun sortRange (lo, hi, c) =
        let
          val xs = List.tabulate (hi - lo + 1, fn i => Array.sub (arr, lo + i))
          fun cmp (a, b) =
            case Int.compare (chan c a, chan c b) of
                EQUAL =>
                  let val (ar, ag, ab) = a and (br, bg, bb) = b
                  in Int.compare (rgbKey (ar, ag, ab), rgbKey (br, bg, bb)) end
              | other => other
          val sorted = msort cmp xs
        in
          List.foldl (fn (x, i) => (Array.update (arr, i, x); i + 1)) lo sorted; ()
        end
      fun spread (lo, hi) = if hi <= lo then ~1 else #2 (rangeOf (lo, hi))
      fun splitBoxes boxes =
        if length boxes >= maxColors then boxes
        else
          let
            fun pick (i, bI, bS, []) = bI
              | pick (i, bI, bS, b :: bs) =
                  let val s = spread b
                  in if s > bS then pick (i + 1, i, s, bs) else pick (i + 1, bI, bS, bs) end
            val bi = pick (0, ~1, ~1, boxes)
          in
            if bi < 0 then boxes
            else
              let
                val (lo, hi) = List.nth (boxes, bi)
                val (c, _) = rangeOf (lo, hi)
                val () = sortRange (lo, hi, c)
                val mid = (lo + hi) div 2
                val newBoxes =
                  List.take (boxes, bi) @ [(lo, mid), (mid + 1, hi)]
                  @ List.drop (boxes, bi + 1)
              in splitBoxes newBoxes end
          end
      fun avg (lo, hi) =
        let
          fun lp i (sr, sg, sb) =
            if i > hi then (sr, sg, sb)
            else
              let val (r, g, b) = Array.sub (arr, i)
              in lp (i + 1) (sr + r, sg + g, sb + b) end
          val cnt = hi - lo + 1
          val (sr, sg, sb) = lp lo (0, 0, 0)
        in
          ( (sr + cnt div 2) div cnt
          , (sg + cnt div 2) div cnt
          , (sb + cnt div 2) div cnt )
        end
      val boxes = splitBoxes [(0, n - 1)]
    in
      List.map avg boxes
    end

  (* ---------- LZW compression (GIF variant) ---------- *)

  (* dictionary for encoding: key = prefixCode*256 + suffixIndex, via a fixed
     open-addressing table reset on each clear. *)
  fun lzwEncode (minCodeSize, indices : Word8Array.array, n : int) =
    let
      val out = newBuf ()
      val clearCode = pow2 minCodeSize
      val eofCode = clearCode + 1

      val hsize = 9973
      val htKeys = Array.array (hsize, ~1)
      val htVals = Array.array (hsize, 0)
      fun htReset () =
        let fun lp i = if i >= hsize then () else (Array.update (htKeys, i, ~1); lp (i + 1))
        in lp 0 end
      fun htFind key =
        let
          fun probe h =
            let val k = Array.sub (htKeys, h)
            in if k = ~1 then NONE
               else if k = key then SOME (Array.sub (htVals, h))
               else probe ((h + 1) mod hsize)
            end
        in probe (key mod hsize) end
      fun htInsert (key, v) =
        let
          fun probe h =
            if Array.sub (htKeys, h) = ~1
            then (Array.update (htKeys, h, key); Array.update (htVals, h, v))
            else probe ((h + 1) mod hsize)
        in probe (key mod hsize) end

      (* bit writer, LSB-first *)
      val acc = ref 0w0
      val nbits = ref 0
      fun flushBytes () =
        if !nbits >= 8 then
          ( pushInt out (Word.toInt (Word.andb (!acc, 0wxFF)))
          ; acc := Word.>> (!acc, 0w8)
          ; nbits := !nbits - 8
          ; flushBytes () )
        else ()
      fun emit (code, width) =
        ( acc := Word.orb (!acc, Word.<< (Word.fromInt code, Word.fromInt (!nbits)))
        ; nbits := !nbits + width
        ; flushBytes () )
      fun emitFinal () =
        if !nbits > 0 then (pushInt out (Word.toInt (Word.andb (!acc, 0wxFF))); acc := 0w0; nbits := 0)
        else ()

      val codeSize = ref (minCodeSize + 1)
      val next = ref (clearCode + 2)
      fun resetDict () =
        (htReset (); codeSize := minCodeSize + 1; next := clearCode + 2)

      val cur = ref 0
    in
      if n = 0 then bufToVector out
      else
        let
          val () = resetDict ()
          val () = emit (clearCode, !codeSize)
          val () = cur := Word8.toInt (Word8Array.sub (indices, 0))
          fun loop i =
            if i >= n then ()
            else
              let
                val k = Word8.toInt (Word8Array.sub (indices, i))
                val key = !cur * 256 + k
              in
                case htFind key of
                    SOME c => (cur := c; loop (i + 1))
                  | NONE =>
                      ( emit (!cur, !codeSize)
                      (* Width bump follows the giflib/spec convention: decide
                         using the pre-insert `next` AFTER emitting, so the wider
                         width takes effect only for subsequent codes. *)
                      ; if !next >= pow2 (!codeSize) andalso !codeSize < 12
                        then codeSize := !codeSize + 1 else ()
                      ; if !next < 4096 then
                          ( htInsert (key, !next); next := !next + 1 )
                        else
                          ( emit (clearCode, !codeSize); resetDict () )
                      ; cur := k
                      ; loop (i + 1) )
              end
          val () = loop 1
          val () = emit (!cur, !codeSize)
          val () = emit (eofCode, !codeSize)
          val () = emitFinal ()
        in
          bufToVector out
        end
    end

  (* write a raw LZW byte stream as GIF data sub-blocks (<=255 bytes each)
     followed by the block terminator. *)
  fun writeSubBlocks (out, lzw : Word8Vector.vector) =
    let
      val total = Word8Vector.length lzw
      fun loop off =
        if off >= total then pushInt out 0
        else
          let val chunk = Int.min (255, total - off)
          in
            pushInt out chunk;
            let
              fun cp j = if j >= chunk then ()
                         else (pushByte out (Word8Vector.sub (lzw, off + j)); cp (j + 1))
            in cp 0 end;
            loop (off + chunk)
          end
    in loop 0 end

  (* ---------- palette construction ---------- *)

  fun buildPalette (frames : frame list, width, height) =
    let
      val seen = newMap ()
      val distinct = newIBuf ()
      fun scan ({ image = { data, ... }, delayCs = _ } : frame) =
        let
          val npx = width * height
          fun lp p =
            if p >= npx then ()
            else
              let
                val base = p * 4
                val r = Word8.toInt (Word8Vector.sub (data, base))
                val g = Word8.toInt (Word8Vector.sub (data, base + 1))
                val b = Word8.toInt (Word8Vector.sub (data, base + 2))
                val key = rgbKey (r, g, b)
              in
                if mapInsert seen (key, 0) then ipush distinct key else ();
                lp (p + 1)
              end
        in lp 0 end
      val () = List.app scan frames
      val nDistinct = #len distinct
    in
      if !nDistinct <= 256 then
        let
          val keys =
            List.tabulate (!nDistinct, fn i => Array.sub (!(#data distinct), i))
          val sorted = msort Int.compare keys
          val pal = List.map (fn k => (keyR k, keyG k, keyB k)) sorted
          val memo = newMap ()
          val () = ignore (List.foldl (fn (k, i) => (mapPut memo (k, i); i + 1)) 0 sorted)
        in
          (pal, memo)
        end
      else
        let
          val colors =
            List.tabulate (!nDistinct,
              fn i => let val k = Array.sub (!(#data distinct), i)
                      in (keyR k, keyG k, keyB k) end)
          val pal = medianCut (colors, 256)
          val memo = newMap ()
        in
          (pal, memo)
        end
    end

  (* ---------- top-level encode ---------- *)

  fun encode { width, height, frames, loop } =
    let
      val () = if width <= 0 orelse height <= 0
               then raise Gif "width/height must be positive" else ()
      val () = if width > 65535 orelse height > 65535
               then raise Gif "dimensions exceed 65535" else ()
      val () = if null frames then raise Gif "need at least one frame" else ()
      val () = if loop < 0 orelse loop > 65535
               then raise Gif "loop out of range" else ()
      val () =
        List.app (fn { image = { width = w, height = h, data } : Image.image, delayCs } =>
          ( if w <> width orelse h <> height
            then raise Gif "frame size mismatch" else ()
          ; if Word8Vector.length data <> 4 * width * height
            then raise Gif "frame data length mismatch" else ()
          ; if delayCs < 0 orelse delayCs > 65535
            then raise Gif "delay out of range" else () )) frames

      val (palette, memo) = buildPalette (frames, width, height)
      val palArr = Array.fromList palette
      val nPal = Array.length palArr
      val () = if nPal < 1 orelse nPal > 256 then raise Gif "bad palette" else ()

      (* nearest palette index for an rgb key, memoized *)
      fun indexOf key =
        case mapFind memo key of
            SOME i => i
          | NONE =>
              let
                val r = keyR key and g = keyG key and b = keyB key
                fun lp i bestI bestD =
                  if i >= nPal then bestI
                  else
                    let
                      val (pr, pg, pb) = Array.sub (palArr, i)
                      val dr = r - pr and dg = g - pg and db = b - pb
                      val d = dr * dr + dg * dg + db * db
                    in
                      if d < bestD then lp (i + 1) i d else lp (i + 1) bestI bestD
                    end
                val idx = lp 0 0 1000000000
              in mapPut memo (key, idx); idx end

      (* color-table bit depth *)
      fun bitsFor m = if m <= 2 then 1 else 1 + bitsFor ((m + 1) div 2)
      val bitDepth = Int.max (1, bitsFor nPal)
      val gctEntries = pow2 bitDepth
      val gctSizeField = bitDepth - 1
      val minCodeSize = Int.max (2, bitDepth)

      val out = newBuf ()

      (* header *)
      val () = pushString out "GIF89a"
      (* logical screen descriptor *)
      val () = pushU16 out width
      val () = pushU16 out height
      val () = pushInt out (128 + gctSizeField * 16 + gctSizeField) (* GCT flag + color res + size *)
      val () = pushInt out 0   (* background color index *)
      val () = pushInt out 0   (* pixel aspect ratio *)
      (* global color table, padded to gctEntries *)
      val () =
        let
          fun emitEntry i =
            if i >= gctEntries then ()
            else
              ( if i < nPal then
                  let val (r, g, b) = Array.sub (palArr, i)
                  in pushInt out r; pushInt out g; pushInt out b end
                else (pushInt out 0; pushInt out 0; pushInt out 0)
              ; emitEntry (i + 1) )
        in emitEntry 0 end

      (* NETSCAPE2.0 application extension: loop count *)
      val () =
        ( pushInt out 0x21; pushInt out 0xFF; pushInt out 11
        ; pushString out "NETSCAPE2.0"
        ; pushInt out 3; pushInt out 1; pushU16 out loop; pushInt out 0 )

      (* per-frame blocks *)
      fun emitFrame ({ image = { data, ... }, delayCs } : frame) =
        let
          (* graphic control extension *)
          val () =
            ( pushInt out 0x21; pushInt out 0xF9; pushInt out 4
            ; pushInt out 4              (* disposal method 1, no transparency *)
            ; pushU16 out delayCs
            ; pushInt out 0              (* transparent color index (unused) *)
            ; pushInt out 0 )
          (* image descriptor *)
          val () =
            ( pushInt out 0x2C
            ; pushU16 out 0; pushU16 out 0
            ; pushU16 out width; pushU16 out height
            ; pushInt out 0 )           (* no local color table, no interlace *)
          (* indices *)
          val npx = width * height
          val idx = Word8Array.array (npx, 0w0)
          val () =
            let
              fun lp p =
                if p >= npx then ()
                else
                  let
                    val base = p * 4
                    val r = Word8.toInt (Word8Vector.sub (data, base))
                    val g = Word8.toInt (Word8Vector.sub (data, base + 1))
                    val b = Word8.toInt (Word8Vector.sub (data, base + 2))
                  in
                    Word8Array.update (idx, p, Word8.fromInt (indexOf (rgbKey (r, g, b))));
                    lp (p + 1)
                  end
            in lp 0 end
          val () = pushInt out minCodeSize
          val lzw = lzwEncode (minCodeSize, idx, npx)
          val () = writeSubBlocks (out, lzw)
        in () end
      val () = List.app emitFrame frames

      (* trailer *)
      val () = pushInt out 0x3B
    in
      bufToVector out
    end
end
