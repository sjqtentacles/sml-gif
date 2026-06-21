(* inflate.sml

   DEFLATE/zlib/gzip decoder + CRC32/Adler32, RFC 1950/1951/1952.

   Design: a mutable bit reader over the input vector (LSB-first within each
   byte, as DEFLATE specifies), Huffman tables represented as (count,symbol)
   canonical-code lookups, and a growable output byte buffer that also serves
   as the sliding window (back-references index into already-produced output).*)

structure Inflate :> INFLATE =
struct
  exception Inflate of string

  structure W8 = Word8
  structure W8V = Word8Vector
  structure W32 = Word32

  fun b2w (b : Word8.word) : Word32.word = W32.fromLargeWord (W8.toLargeWord b)

  (* ---------- CRC-32 (IEEE, reflected, poly 0xEDB88320) ---------- *)
  val crcTable : Word32.word vector =
    Vector.tabulate (256, fn n =>
      let
        fun loop (c, 0) = c
          | loop (c, k) =
              let
                val c' =
                  if W32.andb (c, 0w1) = 0w1
                  then W32.xorb (0wxEDB88320, W32.>> (c, 0w1))
                  else W32.>> (c, 0w1)
              in loop (c', k - 1) end
      in
        loop (W32.fromInt n, 8)
      end)

  fun crc32 v =
    let
      val c =
        W8V.foldl
          (fn (byte, c) =>
             let val idx = W32.toInt (W32.andb (W32.xorb (c, b2w byte), 0wxFF))
             in W32.xorb (Vector.sub (crcTable, idx), W32.>> (c, 0w8)) end)
          0wxFFFFFFFF v
    in
      W32.xorb (c, 0wxFFFFFFFF)
    end

  (* ---------- Adler-32 ---------- *)
  fun adler32 v =
    let
      val modAdler = 65521
      val (a, b) =
        W8V.foldl
          (fn (byte, (a, b)) =>
             let val a' = (a + W8.toInt byte) mod modAdler
             in (a', (b + a') mod modAdler) end)
          (1, 0) v
    in
      W32.orb (W32.<< (W32.fromInt b, 0w16), W32.fromInt a)
    end

  (* ---------- bit reader ---------- *)
  (* state: input vector, byte position, current bit buffer, #bits in buffer *)
  type reader = { data : W8V.vector, pos : int ref, bitbuf : int ref, bitcnt : int ref }

  fun newReader data = { data = data, pos = ref 0, bitbuf = ref 0, bitcnt = ref 0 }

  fun nextByte (r : reader) =
    let val p = !(#pos r)
    in if p >= W8V.length (#data r) then raise Inflate "unexpected end of input"
       else (#pos r := p + 1; W8.toInt (W8V.sub (#data r, p)))
    end

  (* read n bits, LSB-first *)
  fun getBits (r : reader) n =
    let
      fun ensure () =
        if !(#bitcnt r) >= n then ()
        else ( #bitbuf r := !(#bitbuf r) + nextByte r * (Word.toInt (Word.<< (0w1, Word.fromInt (!(#bitcnt r)))))
             ; #bitcnt r := !(#bitcnt r) + 8
             ; ensure () )
      val () = if n = 0 then () else ensure ()
      val mask = Word.toInt (Word.<< (0w1, Word.fromInt n)) - 1
      val v = Word.toInt (Word.andb (Word.fromInt (!(#bitbuf r)), Word.fromInt mask))
    in
      #bitbuf r := Word.toInt (Word.>> (Word.fromInt (!(#bitbuf r)), Word.fromInt n));
      #bitcnt r := !(#bitcnt r) - n;
      v
    end

  fun alignByte (r : reader) = ( #bitbuf r := 0; #bitcnt r := 0 )

  (* ---------- output buffer (also the sliding window) ---------- *)
  type outbuf = { arr : W8.word array ref, len : int ref }

  fun newOut () : outbuf = { arr = ref (Array.array (1024, 0w0)), len = ref 0 }

  fun ensureCap (ob : outbuf) extra =
    let
      val cap = Array.length (!(#arr ob))
      val need = !(#len ob) + extra
    in
      if need <= cap then ()
      else
        let
          fun grow c = if c >= need then c else grow (c * 2)
          val newCap = grow (cap * 2)
          val a' = Array.array (newCap, 0w0)
        in
          Array.copy { src = !(#arr ob), dst = a', di = 0 };
          #arr ob := a'
        end
    end

  fun pushByte (ob : outbuf) (b : W8.word) =
    ( ensureCap ob 1
    ; Array.update (!(#arr ob), !(#len ob), b)
    ; #len ob := !(#len ob) + 1 )

  fun copyMatch (ob : outbuf) (dist, len) =
    let
      val start = !(#len ob) - dist
    in
      if start < 0 then raise Inflate "invalid back-reference distance"
      else
        let
          fun loop 0 = ()
            | loop k =
                let val src = !(#len ob) - dist
                in pushByte ob (Array.sub (!(#arr ob), src)); loop (k - 1) end
        in loop len end
    end

  fun outToVector (ob : outbuf) =
    W8V.tabulate (!(#len ob), fn i => Array.sub (!(#arr ob), i))

  (* ---------- Huffman tables ---------- *)
  (* A canonical Huffman decoder built from a list of code lengths. We use the
     classic counts/offsets scheme (as in puff.c): `count.(len)` = number of
     codes of that length, `symbols` = symbols sorted by (len, symbol). *)
  type huffman = { counts : int array, symbols : int array }

  val maxBits = 15

  fun buildHuffman (lengths : int vector) : huffman =
    let
      val n = Vector.length lengths
      val counts = Array.array (maxBits + 1, 0)
      val () = Vector.app
        (fn l => if l > 0 then Array.update (counts, l, Array.sub (counts, l) + 1) else ())
        lengths
      (* offsets for each length *)
      val offsets = Array.array (maxBits + 2, 0)
      fun fillOffsets l =
        if l > maxBits then ()
        else ( Array.update (offsets, l + 1, Array.sub (offsets, l) + Array.sub (counts, l))
             ; fillOffsets (l + 1) )
      val () = fillOffsets 1
      val symbols = Array.array (n, 0)
      fun place s =
        if s >= n then ()
        else
          let val l = Vector.sub (lengths, s)
          in (if l > 0 then
                ( Array.update (symbols, Array.sub (offsets, l), s)
                ; Array.update (offsets, l, Array.sub (offsets, l) + 1) )
              else ())
           ; place (s + 1)
          end
      val () = place 0
    in
      { counts = counts, symbols = symbols }
    end

  (* decode one symbol using bit-at-a-time canonical decoding *)
  fun decodeSym (r : reader) ({counts, symbols} : huffman) =
    let
      fun loop (len, code, first, index) =
        if len > maxBits then raise Inflate "invalid Huffman code"
        else
          let
            val code = code + getBits r 1     (* one more (LSB) bit *)
            val cnt = Array.sub (counts, len)
          in
            if code - first < cnt
            then Array.sub (symbols, index + (code - first))
            else
              loop (len + 1, code * 2, (first + cnt) * 2, index + cnt)
          end
    in
      loop (1, 0, 0, 0)
    end

  (* length / distance base tables (RFC 1951) *)
  val lenBase = Vector.fromList [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,
                  35,43,51,59,67,83,99,115,131,163,195,227,258]
  val lenExtra = Vector.fromList [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,
                   3,3,3,3,4,4,4,4,5,5,5,5,0]
  val distBase = Vector.fromList [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,
                   257,385,513,769,1025,1537,2049,3073,4097,6145,
                   8193,12289,16385,24577]
  val distExtra = Vector.fromList [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,
                    9,9,10,10,11,11,12,12,13,13]

  fun inflateBlockData (r : reader) (ob : outbuf) (litHuff, distHuff) =
    let
      fun loop () =
        let val sym = decodeSym r litHuff
        in
          if sym = 256 then ()                 (* end of block *)
          else if sym < 256 then ( pushByte ob (W8.fromInt sym); loop () )
          else
            let
              val li = sym - 257
              val () = if li >= Vector.length lenBase
                       then raise Inflate "invalid length symbol" else ()
              val len = Vector.sub (lenBase, li) + getBits r (Vector.sub (lenExtra, li))
              val dsym = decodeSym r distHuff
              val () = if dsym >= Vector.length distBase
                       then raise Inflate "invalid distance symbol" else ()
              val dist = Vector.sub (distBase, dsym) + getBits r (Vector.sub (distExtra, dsym))
            in
              copyMatch ob (dist, len); loop ()
            end
        end
    in
      loop ()
    end

  (* fixed Huffman tables (RFC 1951 3.2.6) *)
  val fixedLit : huffman =
    buildHuffman (Vector.tabulate (288, fn s =>
      if s < 144 then 8 else if s < 256 then 9 else if s < 280 then 7 else 8))
  val fixedDist : huffman =
    buildHuffman (Vector.tabulate (30, fn _ => 5))

  fun storedBlock (r : reader) (ob : outbuf) =
    let
      val () = alignByte r
      val len = nextByte r + nextByte r * 256
      val nlen = nextByte r + nextByte r * 256
      val () = if W32.andb (W32.fromInt len, 0wxFFFF)
                  <> W32.andb (W32.notb (W32.fromInt nlen), 0wxFFFF)
               then raise Inflate "stored block length mismatch" else ()
      fun loop 0 = ()
        | loop k = ( pushByte ob (W8.fromInt (nextByte r)); loop (k - 1) )
    in
      loop len
    end

  fun dynamicBlock (r : reader) (ob : outbuf) =
    let
      val hlit = getBits r 5 + 257
      val hdist = getBits r 5 + 1
      val hclen = getBits r 4 + 4
      val clOrder = Vector.fromList [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
      val clLens = Array.array (19, 0)
      fun readCl i =
        if i >= hclen then ()
        else ( Array.update (clLens, Vector.sub (clOrder, i), getBits r 3)
             ; readCl (i + 1) )
      val () = readCl 0
      val clHuff = buildHuffman (Vector.tabulate (19, fn i => Array.sub (clLens, i)))
      (* read hlit+hdist code lengths using the code-length Huffman *)
      val total = hlit + hdist
      val lens = Array.array (total, 0)
      fun readLens i =
        if i >= total then ()
        else
          let val sym = decodeSym r clHuff
          in
            if sym < 16 then ( Array.update (lens, i, sym); readLens (i + 1) )
            else if sym = 16 then
              let
                val () = if i = 0 then raise Inflate "bad repeat at start" else ()
                val prev = Array.sub (lens, i - 1)
                val rep = getBits r 2 + 3
                fun put (k, j) = if k = 0 orelse i + j >= total then j
                                 else ( Array.update (lens, i + j, prev); put (k - 1, j + 1) )
                val filled = put (rep, 0)
              in readLens (i + filled) end
            else if sym = 17 then
              let val rep = getBits r 3 + 3
              in readLens (i + rep) end           (* zeros (already 0) *)
            else (* 18 *)
              let val rep = getBits r 7 + 11
              in readLens (i + rep) end
          end
      val () = readLens 0
      val litHuff = buildHuffman (Vector.tabulate (hlit, fn i => Array.sub (lens, i)))
      val distHuff = buildHuffman (Vector.tabulate (hdist, fn i => Array.sub (lens, hlit + i)))
    in
      inflateBlockData r ob (litHuff, distHuff)
    end

  fun inflateInto (r : reader) (ob : outbuf) =
    let
      fun loop () =
        let
          val bfinal = getBits r 1
          val btype = getBits r 2
        in
          (case btype of
               0 => storedBlock r ob
             | 1 => inflateBlockData r ob (fixedLit, fixedDist)
             | 2 => dynamicBlock r ob
             | _ => raise Inflate "invalid block type");
          if bfinal = 1 then () else loop ()
        end
    in
      loop ()
    end

  fun inflate data =
    let
      val r = newReader data
      val ob = newOut ()
    in
      inflateInto r ob; outToVector ob
    end

  (* ---------- zlib wrapper (RFC 1950) ---------- *)
  fun zlib data =
    let
      val n = W8V.length data
      val () = if n < 6 then raise Inflate "zlib: too short" else ()
      val cmf = W8.toInt (W8V.sub (data, 0))
      val flg = W8.toInt (W8V.sub (data, 1))
      val () = if Int.mod (cmf, 16) <> 8 then raise Inflate "zlib: not DEFLATE" else ()
      val () = if Int.mod (cmf * 256 + flg, 31) <> 0
               then raise Inflate "zlib: bad header check" else ()
      val hasDict = (Int.mod (flg div 32, 2) = 1)
      val dataStart = if hasDict then 6 else 2
      val body = W8V.tabulate (n - dataStart - 4, fn i => W8V.sub (data, dataStart + i))
      val out = inflate body
      (* trailer: 4-byte big-endian Adler-32 *)
      fun be i = W32.fromInt (W8.toInt (W8V.sub (data, n - 4 + i)))
      val want = W32.orb (W32.<< (be 0, 0w24),
                  W32.orb (W32.<< (be 1, 0w16),
                   W32.orb (W32.<< (be 2, 0w8), be 3)))
      val () = if adler32 out <> want
               then raise Inflate "zlib: Adler-32 mismatch" else ()
    in
      out
    end

  (* ---------- gzip wrapper (RFC 1952) ---------- *)
  fun gunzip data =
    let
      val n = W8V.length data
      val () = if n < 18 then raise Inflate "gzip: too short" else ()
      val () = if W8V.sub (data, 0) <> 0wx1F orelse W8V.sub (data, 1) <> 0wx8B
               then raise Inflate "gzip: bad magic" else ()
      val () = if W8V.sub (data, 2) <> 0wx08
               then raise Inflate "gzip: not DEFLATE" else ()
      val flg = W8.toInt (W8V.sub (data, 3))
      val pos = ref 10
      fun skipZeroTerm () =
        if !pos >= n then raise Inflate "gzip: truncated header"
        else if W8V.sub (data, !pos) = 0w0 then pos := !pos + 1
        else ( pos := !pos + 1; skipZeroTerm () )
      (* FEXTRA *)
      val () = if Int.mod (flg div 4, 2) = 1 then
                 let val xlen = W8.toInt (W8V.sub (data, !pos))
                                + W8.toInt (W8V.sub (data, !pos + 1)) * 256
                 in pos := !pos + 2 + xlen end
               else ()
      val () = if Int.mod (flg div 8, 2) = 1 then skipZeroTerm () else ()   (* FNAME *)
      val () = if Int.mod (flg div 16, 2) = 1 then skipZeroTerm () else ()  (* FCOMMENT *)
      val () = if Int.mod (flg div 2, 2) = 1 then pos := !pos + 2 else ()   (* FHCRC *)
      val dataStart = !pos
      val body = W8V.tabulate (n - dataStart - 8, fn i => W8V.sub (data, dataStart + i))
      val out = inflate body
      (* trailer: 4-byte little-endian CRC-32, then 4-byte ISIZE *)
      fun le base i = W32.fromInt (W8.toInt (W8V.sub (data, base + i)))
      val crcWant = W32.orb (le (n - 8) 0,
                     W32.orb (W32.<< (le (n - 8) 1, 0w8),
                      W32.orb (W32.<< (le (n - 8) 2, 0w16), W32.<< (le (n - 8) 3, 0w24))))
      val () = if crc32 out <> crcWant
               then raise Inflate "gzip: CRC-32 mismatch" else ()
    in
      out
    end
end

