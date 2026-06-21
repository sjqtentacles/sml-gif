(* support.sml -- shared helpers for the sml-gif tests.

   The centerpiece is an independent GIF89a decoder, ported from the canonical
   giflib LZW logic, used to round-trip-verify the encoder: encode frames, decode
   the bytes back, and compare pixels. A self-consistent encoder/decoder pair is
   not enough on its own (both could share a bug), so the example demo is also
   validated against a third-party decoder; here we test structure + round trip. *)

structure Support =
struct
  structure G = Gif

  fun pow2 k = Word.toInt (Word.<< (0w1, Word.fromInt k))

  (* ---- build an RGBA image from a (x,y) -> (r,g,b) function (opaque) ---- *)
  fun mkImage (w, h, f) : Image.image =
    let
      val data = Word8Array.array (4 * w * h, 0w0)
      fun lp p =
        if p >= w * h then ()
        else
          let
            val x = p mod w and y = p div w
            val (r, g, b) = f (x, y)
            val base = p * 4
          in
            Word8Array.update (data, base, Word8.fromInt r);
            Word8Array.update (data, base + 1, Word8.fromInt g);
            Word8Array.update (data, base + 2, Word8.fromInt b);
            Word8Array.update (data, base + 3, 0w255);
            lp (p + 1)
          end
    in
      lp 0;
      { width = w, height = h, data = Word8Array.vector data }
    end

  fun pixel (img : Image.image) (x, y) =
    let
      val base = (y * #width img + x) * 4
      val d = #data img
    in
      ( Word8.toInt (Word8Vector.sub (d, base))
      , Word8.toInt (Word8Vector.sub (d, base + 1))
      , Word8.toInt (Word8Vector.sub (d, base + 2)) )
    end

  (* ---- byte helpers ---- *)
  fun byteAt (v, i) = Word8.toInt (Word8Vector.sub (v, i))
  fun u16At (v, i) = byteAt (v, i) + byteAt (v, i + 1) * 256
  fun ascii (v, i, n) =
    CharVector.tabulate (n, fn j => Char.chr (byteAt (v, i + j)))

  (* ---- decoded GIF ---- *)
  type dframe = { delayCs : int, image : Image.image }
  type decoded =
    { version : string
    , width : int
    , height : int
    , loop : int option
    , gctSize : int
    , frames : dframe list }

  exception Decode of string

  (* giflib-style LZW decode of one image's data into `npx` palette indices *)
  fun lzwDecode (minCodeSize, data : Word8Vector.vector, npx) =
    let
      val clearCode = pow2 minCodeSize
      val eofCode = clearCode + 1
      val LZ_MAX = 4095
      val dataLen = Word8Vector.length data

      val bytePos = ref 0
      val shiftDWord = ref 0w0
      val shiftState = ref 0
      val runningBits = ref (minCodeSize + 1)
      val maxCode1 = ref (pow2 (minCodeSize + 1))
      val runningCode = ref (eofCode + 1)

      fun readCode () =
        let
          fun fill () =
            if !shiftState < !runningBits andalso !bytePos < dataLen then
              ( shiftDWord := Word.orb (!shiftDWord,
                   Word.<< (Word.fromInt (byteAt (data, !bytePos)),
                            Word.fromInt (!shiftState)))
              ; shiftState := !shiftState + 8
              ; bytePos := !bytePos + 1
              ; fill () )
            else ()
          val () = fill ()
          val mask = Word.fromInt (pow2 (!runningBits) - 1)
          val code = Word.toInt (Word.andb (!shiftDWord, mask))
        in
          shiftDWord := Word.>> (!shiftDWord, Word.fromInt (!runningBits));
          shiftState := !shiftState - !runningBits;
          runningCode := !runningCode + 1;
          if !runningCode > !maxCode1 andalso !runningBits < 12
          then (maxCode1 := !maxCode1 * 2; runningBits := !runningBits + 1)
          else ();
          code
        end

      val out = Word8Array.array (npx, 0w0)
      val outPos = ref 0
      val prefix = Array.array (4096, ~1)
      val suffix = Array.array (4096, 0)
      val stack = Array.array (4096, 0)
      val stackPtr = ref 0
      val lastCode = ref ~1

      fun resetTables () =
        ( let fun lp i = if i >= 4096 then () else (Array.update (prefix, i, ~1); lp (i + 1))
          in lp 0 end
        ; runningBits := minCodeSize + 1
        ; maxCode1 := pow2 (minCodeSize + 1)
        ; runningCode := eofCode + 1 )

      fun getPrefixChar code =
        let
          fun lp (c, i) =
            if c > clearCode andalso i <= LZ_MAX then lp (Array.sub (prefix, c), i + 1)
            else c
          val c = lp (code, 0)
        in if c > clearCode then 0 else c end

      fun pushOut w = (Word8Array.update (out, !outPos, Word8.fromInt w); outPos := !outPos + 1)
      fun drainStack () =
        if !stackPtr <> 0 andalso !outPos < npx then
          (stackPtr := !stackPtr - 1; pushOut (Array.sub (stack, !stackPtr)); drainStack ())
        else ()

      fun loop () =
        if !outPos >= npx then ()
        else
          let val crnt = readCode ()
          in
            if crnt = eofCode then ()
            else if crnt = clearCode then (resetTables (); lastCode := ~1; loop ())
            else
              ( if crnt < clearCode then pushOut crnt
                else
                  let val crntPrefix = ref crnt
                  in
                    if Array.sub (prefix, crnt) = ~1 then
                      ( crntPrefix := !lastCode
                      ; Array.update (stack, !stackPtr, getPrefixChar (!lastCode))
                      ; stackPtr := !stackPtr + 1 )
                    else ();
                    let
                      fun walk () =
                        if !crntPrefix > clearCode andalso !crntPrefix <= LZ_MAX then
                          ( Array.update (stack, !stackPtr, Array.sub (suffix, !crntPrefix))
                          ; stackPtr := !stackPtr + 1
                          ; crntPrefix := Array.sub (prefix, !crntPrefix)
                          ; walk () )
                        else ()
                    in walk () end;
                    Array.update (stack, !stackPtr, !crntPrefix);
                    stackPtr := !stackPtr + 1;
                    drainStack ()
                  end
              ; if !lastCode <> ~1 andalso !runningCode - 2 >= 0
                   andalso !runningCode - 2 <= LZ_MAX
                   andalso Array.sub (prefix, !runningCode - 2) = ~1 then
                  ( Array.update (prefix, !runningCode - 2, !lastCode)
                  ; if crnt = !runningCode - 2 then
                      Array.update (suffix, !runningCode - 2, getPrefixChar (!lastCode))
                    else
                      Array.update (suffix, !runningCode - 2, getPrefixChar crnt) )
                else ()
              ; lastCode := crnt
              ; loop () )
          end
    in
      resetTables ();
      loop ();
      out
    end

  fun decodeGif (v : Word8Vector.vector) : decoded =
    let
      val pos = ref 0
      fun u8 () = let val b = byteAt (v, !pos) in pos := !pos + 1; b end
      fun u16 () = let val lo = u8 () val hi = u8 () in lo + hi * 256 end
      fun skip n = pos := !pos + n

      val version = ascii (v, 0, 6)
      val () = pos := 6
      val () = if String.isPrefix "GIF" version then () else raise Decode "bad magic"
      val width = u16 ()
      val height = u16 ()
      val packed = u8 ()
      val _ = u8 ()   (* background *)
      val _ = u8 ()   (* aspect ratio *)
      val hasGct = packed >= 128
      val gctSize = if hasGct then pow2 ((packed mod 8) + 1) else 0
      val gct = Array.array (Int.max (gctSize, 1) * 3, 0w0)
      val () =
        if hasGct then
          let fun lp i = if i >= gctSize * 3 then () else (Array.update (gct, i, Word8.fromInt (u8 ())); lp (i + 1))
          in lp 0 end
        else ()

      val loopRef = ref (NONE : int option)
      val framesRef = ref ([] : dframe list)
      val pendingDelay = ref 0

      fun readSubBlocks () =
        let
          fun gather acc =
            let val len = u8 ()
            in
              if len = 0 then acc
              else
                let
                  val arr = Word8Array.array (len, 0w0)
                  fun rd j = if j >= len then () else (Word8Array.update (arr, j, Word8.fromInt (u8 ())); rd (j + 1))
                  val () = rd 0
                in gather (acc @ [Word8Array.vector arr]) end
            end
        in Word8Vector.concat (gather []) end

      fun readImage () =
        let
          val _ = u16 ()  (* left *)
          val _ = u16 ()  (* top *)
          val iw = u16 ()
          val ih = u16 ()
          val ipacked = u8 ()
          val hasLct = ipacked >= 128
          val lctSize = if hasLct then pow2 ((ipacked mod 8) + 1) else 0
          val () = if hasLct then skip (lctSize * 3) else ()
          val minCodeSize = u8 ()
          val data = readSubBlocks ()
          val idx = lzwDecode (minCodeSize, data, iw * ih)
          val table = if hasLct then raise Decode "local color table unsupported in test" else gct
          val img =
            mkImage (iw, ih, fn (x, y) =>
              let
                val ci = Word8.toInt (Word8Array.sub (idx, y * iw + x))
                val base = ci * 3
              in
                ( Word8.toInt (Array.sub (table, base))
                , Word8.toInt (Array.sub (table, base + 1))
                , Word8.toInt (Array.sub (table, base + 2)) )
              end)
        in
          framesRef := !framesRef @ [{ delayCs = !pendingDelay, image = img }];
          pendingDelay := 0
        end

      fun loopBlocks () =
        let val b = u8 ()
        in
          if b = 0x3B then ()              (* trailer *)
          else if b = 0x2C then (readImage (); loopBlocks ())
          else if b = 0x21 then
            let val label = u8 ()
            in
              if label = 0xF9 then
                let
                  val _ = u8 ()            (* block size = 4 *)
                  val _ = u8 ()            (* packed *)
                  val delay = u16 ()
                  val _ = u8 ()            (* transparent index *)
                  val _ = u8 ()            (* terminator *)
                in pendingDelay := delay; loopBlocks () end
              else if label = 0xFF then
                let
                  val size = u8 ()
                  val name = ascii (v, !pos, size)
                  val () = skip size
                  val sub = readSubBlocks ()
                in
                  if String.isPrefix "NETSCAPE" name andalso Word8Vector.length sub >= 3
                  then loopRef := SOME (byteAt (sub, 1) + byteAt (sub, 2) * 256)
                  else ();
                  loopBlocks ()
                end
              else (ignore (readSubBlocks ()); loopBlocks ())
            end
          else raise Decode "unknown block"
        end
      val () = loopBlocks ()
    in
      { version = version, width = width, height = height
      , loop = !loopRef, gctSize = gctSize, frames = !framesRef }
    end

  (* compare two images for exact RGB equality *)
  fun sameImage (a : Image.image, b : Image.image) =
    #width a = #width b andalso #height a = #height b
    andalso
    let
      val n = #width a * #height a
      fun lp p =
        if p >= n then true
        else
          let val x = p mod #width a and y = p div #width a
          in if pixel a (x, y) = pixel b (x, y) then lp (p + 1) else false end
    in lp 0 end
end
