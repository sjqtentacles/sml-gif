(* image.sml

   Implementation of the IMAGE signature: RGBA8 raster plus PNM/BMP/TGA/PNG
   codecs. Decompression for PNG is delegated to the vendored sml-inflate.
   Pure Basis; deterministic and byte-identical across MLton and Poly/ML. *)

structure Image :> IMAGE =
struct
  exception Image of string

  type image = { width : int, height : int, data : Word8Vector.vector }
  type rgba8 = { r : Word8.word, g : Word8.word, b : Word8.word, a : Word8.word }

  structure W8  = Word8
  structure W8V = Word8Vector
  structure W8S = Word8VectorSlice
  structure W32 = Word32

  fun b2i (b : W8.word) = W8.toInt b
  fun i2b (i : int) = W8.fromInt i

  (* --- pixel access --- *)

  fun checkDims { width, height, data } =
    if width < 0 orelse height < 0 then raise Image "negative dimension"
    else if W8V.length data <> 4 * width * height
    then raise Image "data length does not match width*height*4"
    else ()

  fun pixOffset ({ width, height, ... } : image) (x, y) =
    if x < 0 orelse y < 0 orelse x >= width orelse y >= height
    then raise Image "pixel out of range"
    else 4 * (y * width + x)

  fun getPixel (img as { data, ... }) (x, y) =
    let val off = pixOffset img (x, y)
    in { r = W8V.sub (data, off), g = W8V.sub (data, off + 1),
         b = W8V.sub (data, off + 2), a = W8V.sub (data, off + 3) }
    end

  fun setPixel (img as { width, height, data }) (x, y) (p : rgba8) =
    let
      val off = pixOffset img (x, y)
      val data' =
        W8V.mapi
          (fn (i, v) =>
             if i = off then #r p
             else if i = off + 1 then #g p
             else if i = off + 2 then #b p
             else if i = off + 3 then #a p
             else v)
          data
    in { width = width, height = height, data = data' }
    end

  fun fill (w, h) (p : rgba8) =
    if w < 0 orelse h < 0 then raise Image "negative dimension"
    else
      let
        val data =
          W8V.tabulate (4 * w * h,
            fn i => case i mod 4 of
                      0 => #r p | 1 => #g p | 2 => #b p | _ => #a p)
      in { width = w, height = h, data = data } end

  (* --- little-endian / big-endian byte helpers --- *)

  fun u8 (v, i) =
    if i < 0 orelse i >= W8V.length v then raise Image "unexpected end of input"
    else b2i (W8V.sub (v, i))

  fun u16le (v, i) = u8 (v, i) + u8 (v, i + 1) * 256
  fun u32le (v, i) =
    W32.fromInt (u8 (v, i))
    + W32.<< (W32.fromInt (u8 (v, i + 1)), 0w8)
    + W32.<< (W32.fromInt (u8 (v, i + 2)), 0w16)
    + W32.<< (W32.fromInt (u8 (v, i + 3)), 0w24)
  fun u32be (v, i) =
    W32.<< (W32.fromInt (u8 (v, i)), 0w24)
    + W32.<< (W32.fromInt (u8 (v, i + 1)), 0w16)
    + W32.<< (W32.fromInt (u8 (v, i + 2)), 0w8)
    + W32.fromInt (u8 (v, i + 3))

  (* byte-list emitters (little/big endian) *)
  fun le16 n = [i2b (n mod 256), i2b (n div 256 mod 256)]
  fun le32 n =
    [i2b (n mod 256), i2b (n div 256 mod 256),
     i2b (n div 65536 mod 256), i2b (n div 16777216 mod 256)]
  fun le32w (w : W32.word) =
    [i2b (W32.toInt (W32.andb (w, 0wxFF))),
     i2b (W32.toInt (W32.andb (W32.>> (w, 0w8), 0wxFF))),
     i2b (W32.toInt (W32.andb (W32.>> (w, 0w16), 0wxFF))),
     i2b (W32.toInt (W32.andb (W32.>> (w, 0w24), 0wxFF)))]
  fun be32w (w : W32.word) =
    [i2b (W32.toInt (W32.andb (W32.>> (w, 0w24), 0wxFF))),
     i2b (W32.toInt (W32.andb (W32.>> (w, 0w16), 0wxFF))),
     i2b (W32.toInt (W32.andb (W32.>> (w, 0w8), 0wxFF))),
     i2b (W32.toInt (W32.andb (w, 0wxFF)))]

  (* ===================== Netpbm (PPM/PGM) ===================== *)

  local
    fun isWs c = c = 0wx20 orelse c = 0wx09 orelse c = 0wx0A orelse c = 0wx0D

    (* tokenizer over bytes that honors '#'..EOL comments (ascii headers). *)
    fun skipWs (v, i) =
      if i >= W8V.length v then i
      else
        let val c = W8V.sub (v, i) in
          if isWs c then skipWs (v, i + 1)
          else if c = 0wx23 (* '#' *) then
            let
              fun toEol j =
                if j >= W8V.length v orelse W8V.sub (v, j) = 0wx0A then j
                else toEol (j + 1)
            in skipWs (v, toEol (i + 1)) end
          else i
        end

    fun token (v, i) =
      let
        val i = skipWs (v, i)
        fun go j =
          if j >= W8V.length v orelse isWs (W8V.sub (v, j)) then j else go (j + 1)
        val j = go i
      in (Byte.bytesToString (W8S.vector (W8S.slice (v, i, SOME (j - i)))), j)
      end

    fun intTok (v, i) =
      let val (s, j) = token (v, i)
      in case Int.fromString s of
           SOME n => (n, j)
         | NONE => raise Image "pnm: expected integer"
      end
  in
    fun decodePnm v =
      let
        val (magic, i0) = token (v, 0)
        val () = if String.size magic = 2 andalso String.sub (magic, 0) = #"P"
                 then () else raise Image "pnm: bad magic"
        val kind = String.sub (magic, 1)
        val (w, i1) = intTok (v, i0)
        val (h, i2) = intTok (v, i1)
        val (maxv, i3) = intTok (v, i2)
        val () = if maxv <> 255 then raise Image "pnm: only maxval 255 supported" else ()
        val gray = (kind = #"2" orelse kind = #"5")
        val ascii = (kind = #"2" orelse kind = #"3")
        val npix = w * h
        fun mkData readChan =
          W8V.tabulate (4 * npix,
            fn k =>
              let val px = k div 4 val ch = k mod 4 in
                if ch = 3 then 0wxFF
                else if gray then readChan (px, 0)  (* same for r,g,b *)
                else readChan (px, ch)
              end)
      in
        if ascii then
          let
            (* read all samples up front into an int array *)
            val nsamp = if gray then npix else npix * 3
            val arr = Array.array (nsamp, 0)
            fun loop (k, pos) =
              if k >= nsamp then ()
              else let val (n, pos') = intTok (v, pos)
                   in Array.update (arr, k, n); loop (k + 1, pos') end
            val () = loop (0, i3)
            fun read (px, ch) =
              i2b (Array.sub (arr, if gray then px else px * 3 + ch))
          in { width = w, height = h, data = mkData read } end
        else
          let
            (* binary: single whitespace after maxval, then raw bytes *)
            val start = i3 + 1
            val stride = if gray then 1 else 3
            fun read (px, ch) =
              let val off = start + px * stride + (if gray then 0 else ch)
              in if off >= W8V.length v then raise Image "pnm: truncated"
                 else W8V.sub (v, off) end
          in { width = w, height = h, data = mkData read } end
      end

    fun encodePpm (img as { width, height, data }) =
      let
        val () = checkDims img
        val header = Byte.stringToBytes ("P6\n" ^ Int.toString width ^ " "
                       ^ Int.toString height ^ "\n255\n")
        val body = W8V.tabulate (3 * width * height,
                     fn k => let val px = k div 3 val ch = k mod 3
                             in W8V.sub (data, px * 4 + ch) end)
      in W8V.concat [header, body] end

    fun encodePgm (img as { width, height, data }) =
      let
        val () = checkDims img
        val header = Byte.stringToBytes ("P5\n" ^ Int.toString width ^ " "
                       ^ Int.toString height ^ "\n255\n")
        (* luma via integer Rec.601-ish weights, deterministic *)
        fun luma px =
          let
            val r = b2i (W8V.sub (data, px * 4))
            val g = b2i (W8V.sub (data, px * 4 + 1))
            val b = b2i (W8V.sub (data, px * 4 + 2))
          in i2b ((r * 77 + g * 150 + b * 29) div 256) end
        val body = W8V.tabulate (width * height, luma)
      in W8V.concat [header, body] end
  end

  (* ===================== BMP ===================== *)

  local
    val fileHdr = 14
    val infoHdr = 40
  in
    fun decodeBmp v =
      let
        val () = if W8V.length v < fileHdr + infoHdr then raise Image "bmp: too short" else ()
        val () = if u8 (v, 0) <> 0x42 orelse u8 (v, 1) <> 0x4D
                 then raise Image "bmp: bad magic" else ()
        val dataOff = W32.toInt (u32le (v, 10))
        val width = W32.toInt (u32le (v, 18))
        (* height may be negative (top-down); read as signed 32 *)
        val rawH = u32le (v, 22)
        val topDown = W32.andb (rawH, 0wx80000000) <> 0w0
        val height = if topDown
                     then W32.toInt (W32.~ rawH)  (* two's complement magnitude *)
                     else W32.toInt rawH
        val bpp = u16le (v, 28)
        val comp = W32.toInt (u32le (v, 30))
        val () = if comp <> 0 then raise Image "bmp: only uncompressed BI_RGB supported" else ()
        val () = if bpp <> 24 andalso bpp <> 32 then raise Image "bmp: only 24/32-bit supported" else ()
        val bytesPP = bpp div 8
        val rowSize = ((bytesPP * width + 3) div 4) * 4   (* 4-byte aligned *)
        fun srcRow y = if topDown then y else height - 1 - y
        fun read (px, ch) =
          let
            val x = px mod width
            val y = px div width
            val off = dataOff + srcRow y * rowSize + x * bytesPP
            (* BMP stores BGRA; map ch (0=r,1=g,2=b,3=a) to source *)
          in
            case ch of
              0 => i2b (u8 (v, off + 2))
            | 1 => i2b (u8 (v, off + 1))
            | 2 => i2b (u8 (v, off))
            | _ => if bytesPP = 4 then i2b (u8 (v, off + 3)) else 0wxFF
          end
        val data = W8V.tabulate (4 * width * height,
                     fn k => read (k div 4, k mod 4))
      in { width = width, height = height, data = data } end

    fun encodeBmp (img as { width, height, data }) =
      let
        val () = checkDims img
        val rowSize = width * 4
        val imageSize = rowSize * height
        val dataOff = fileHdr + infoHdr
        val fileSize = dataOff + imageSize
        val header =
          [0wx42, 0wx4D]                 (* 'BM' *)
          @ le32 fileSize @ le16 0 @ le16 0 @ le32 dataOff
          @ le32 infoHdr @ le32 width
          @ le32w (W32.~ (W32.fromInt height))  (* negative height => top-down *)
          @ le16 1 @ le16 32 @ le32 0 @ le32 imageSize
          @ le32 2835 @ le32 2835 @ le32 0 @ le32 0
        val hdrV = W8V.fromList header
        (* top-down, BGRA *)
        val body = W8V.tabulate (imageSize,
                     fn k =>
                       let val px = k div 4 val ch = k mod 4 in
                         case ch of
                           0 => W8V.sub (data, px * 4 + 2)
                         | 1 => W8V.sub (data, px * 4 + 1)
                         | 2 => W8V.sub (data, px * 4)
                         | _ => W8V.sub (data, px * 4 + 3)
                       end)
      in W8V.concat [hdrV, body] end
  end

  (* ===================== TGA ===================== *)

  fun decodeTga v =
    let
      val () = if W8V.length v < 18 then raise Image "tga: too short" else ()
      val idLen = u8 (v, 0)
      val cmapType = u8 (v, 1)
      val imgType = u8 (v, 2)
      val () = if imgType <> 2 then raise Image "tga: only uncompressed truecolor (type 2) supported" else ()
      val () = if cmapType <> 0 then raise Image "tga: colormap not supported" else ()
      val width = u16le (v, 12)
      val height = u16le (v, 14)
      val bpp = u8 (v, 16)
      val () = if bpp <> 24 andalso bpp <> 32 then raise Image "tga: only 24/32-bit supported" else ()
      val descriptor = u8 (v, 17)
      val topOrigin = (descriptor div 32) mod 2 = 1   (* bit 5 *)
      val bytesPP = bpp div 8
      val start = 18 + idLen   (* skip image id; no colormap *)
      fun srcRow y = if topOrigin then y else height - 1 - y
      fun read (px, ch) =
        let
          val x = px mod width
          val y = px div width
          val off = start + (srcRow y * width + x) * bytesPP
        in
          case ch of
            0 => i2b (u8 (v, off + 2))
          | 1 => i2b (u8 (v, off + 1))
          | 2 => i2b (u8 (v, off))
          | _ => if bytesPP = 4 then i2b (u8 (v, off + 3)) else 0wxFF
        end
      val data = W8V.tabulate (4 * width * height, fn k => read (k div 4, k mod 4))
    in { width = width, height = height, data = data } end

  fun encodeTga (img as { width, height, data }) =
    let
      val () = checkDims img
      val header =
        [0w0, 0w0, 0w2 (* uncompressed truecolor *), 0w0,0w0,0w0,0w0,0w0]
        @ le16 0 @ le16 0       (* x/y origin *)
        @ le16 width @ le16 height
        @ [0w32, 0w32]          (* 32 bpp, descriptor: 8 alpha bits + top-origin (bit5) *)
      val hdrV = W8V.fromList header
      (* top-down BGRA *)
      val body = W8V.tabulate (4 * width * height,
                   fn k =>
                     let val px = k div 4 val ch = k mod 4 in
                       case ch of
                         0 => W8V.sub (data, px * 4 + 2)
                       | 1 => W8V.sub (data, px * 4 + 1)
                       | 2 => W8V.sub (data, px * 4)
                       | _ => W8V.sub (data, px * 4 + 3)
                     end)
    in W8V.concat [hdrV, body] end

  (* ===================== PNG ===================== *)

  local
    val sig8 = [0wx89,0wx50,0wx4E,0wx47,0wx0D,0wx0A,0wx1A,0wx0A]

    fun paeth (a, b, c) =
      let
        val p = a + b - c
        val pa = Int.abs (p - a)
        val pb = Int.abs (p - b)
        val pc = Int.abs (p - c)
      in
        if pa <= pb andalso pa <= pc then a
        else if pb <= pc then b else c
      end

    fun parseChunks v =
      let
        val () = if W8V.length v < 8 then raise Image "png: too short" else ()
        val () =
          let fun chk i = if i = 8 then ()
                          else if u8 (v, i) = b2i (List.nth (sig8, i)) then chk (i + 1)
                          else raise Image "png: bad signature"
          in chk 0 end
        fun loop (pos, ihdr, idats) =
          if pos + 8 > W8V.length v then raise Image "png: missing IEND"
          else
            let
              val len = W32.toInt (u32be (v, pos))
              val typ = Byte.bytesToString
                          (W8S.vector (W8S.slice (v, pos + 4, SOME 4)))
              val dataPos = pos + 8
              val next = dataPos + len + 4
            in
              case typ of
                "IHDR" => loop (next, SOME dataPos, idats)
              | "IDAT" =>
                  loop (next, ihdr,
                        W8S.vector (W8S.slice (v, dataPos, SOME len)) :: idats)
              | "IEND" => (ihdr, List.rev idats)
              | _ => loop (next, ihdr, idats)
            end
        val (ihdrOpt, idats) = loop (8, NONE, [])
        val ihdr = case ihdrOpt of SOME p => p | NONE => raise Image "png: missing IHDR"
        val width = W32.toInt (u32be (v, ihdr))
        val height = W32.toInt (u32be (v, ihdr + 4))
        val bitDepth = u8 (v, ihdr + 8)
        val colorType = u8 (v, ihdr + 9)
        val interlace = u8 (v, ihdr + 12)
        val () = if interlace <> 0 then raise Image "png: interlaced images not supported" else ()
        val () = if bitDepth <> 8 then raise Image "png: only 8-bit depth supported" else ()
      in (width, height, colorType, W8V.concat idats) end

    fun channelsOf 0 = 1
      | channelsOf 2 = 3
      | channelsOf 3 = 1
      | channelsOf 4 = 2
      | channelsOf 6 = 4
      | channelsOf _ = raise Image "png: unsupported color type"
  in
    fun decodePng v =
      let
        val (width, height, colorType, idat) = parseChunks v
        val raw = Inflate.zlib idat
        val channels = channelsOf colorType
        val bpp = channels
        val stride = width * bpp
        val expect = height * (stride + 1)
        val () = if W8V.length raw < expect then raise Image "png: inflated data too short" else ()
        val out = Array.array (height * stride, 0w0 : W8.word)
        fun outAt (row, i) = Array.sub (out, row * stride + i)
        fun loopRow y =
          if y >= height then ()
          else
            let
              val base = y * (stride + 1)
              val ft = b2i (W8V.sub (raw, base))
              fun recon i =
                if i >= stride then ()
                else
                  let
                    val x = b2i (W8V.sub (raw, base + 1 + i))
                    val a = if i >= bpp then b2i (outAt (y, i - bpp)) else 0
                    val b = if y > 0 then b2i (outAt (y - 1, i)) else 0
                    val c = if y > 0 andalso i >= bpp then b2i (outAt (y - 1, i - bpp)) else 0
                    val r =
                      case ft of
                        0 => x
                      | 1 => x + a
                      | 2 => x + b
                      | 3 => x + (a + b) div 2
                      | 4 => x + paeth (a, b, c)
                      | _ => raise Image "png: bad filter type"
                  in
                    Array.update (out, y * stride + i, i2b (r mod 256));
                    recon (i + 1)
                  end
            in recon 0; loopRow (y + 1) end
        val () = loopRow 0
        fun sample (px, ch) =
          let val off = px * bpp in
            case colorType of
              0 => if ch = 3 then 0wxFF else Array.sub (out, off)
            | 2 => if ch = 3 then 0wxFF else Array.sub (out, off + ch)
            | 4 => if ch = 3 then Array.sub (out, off + 1) else Array.sub (out, off)
            | 6 => Array.sub (out, off + ch)
            | 3 => raise Image "png: palette images not supported"
            | _ => raise Image "png: unsupported color type"
          end
        val data = W8V.tabulate (4 * width * height,
                     fn k => sample (k div 4, k mod 4))
      in { width = width, height = height, data = data } end

    fun encodePng (img as { width, height, data }) =
      let
        val () = checkDims img
        val stride = width * 4
        val rawLen = height * (stride + 1)
        val raw = W8V.tabulate (rawLen,
                    fn k =>
                      let val row = k div (stride + 1)
                          val col = k mod (stride + 1)
                      in if col = 0 then 0w0
                         else W8V.sub (data, row * stride + (col - 1))
                      end)
        val zhdr = [0wx78, 0wx01]
        fun storedBlocks () =
          let
            val n = W8V.length raw
            fun chunks (pos, acc) =
              if n = 0 then
                List.rev ([i2b 1, i2b 0, i2b 0, i2b 0xFF, i2b 0xFF] :: acc)
              else if pos >= n then List.rev acc
              else
                let
                  val len = Int.min (65535, n - pos)
                  val final = pos + len >= n
                  val bfinal = if final then 1 else 0
                  val block =
                    [i2b bfinal] @ le16 len @ le16 (65535 - len)
                    @ List.tabulate (len, fn i => W8V.sub (raw, pos + i))
                in chunks (pos + len, block :: acc) end
          in W8V.fromList (List.concat (chunks (0, []))) end
        val deflate = storedBlocks ()
        val adler = Inflate.adler32 raw
        val zstream = W8V.concat [W8V.fromList zhdr, deflate, W8V.fromList (be32w adler)]
        fun chunk (typ, payload) =
          let
            val tb = Byte.stringToBytes typ
            val body = W8V.concat [tb, payload]
            val crc = Inflate.crc32 body
          in W8V.concat [W8V.fromList (be32w (W32.fromInt (W8V.length payload))),
                         body, W8V.fromList (be32w crc)] end
        val ihdr = W8V.fromList
                     (be32w (W32.fromInt width) @ be32w (W32.fromInt height)
                      @ [0w8, 0w6, 0w0, 0w0, 0w0])
      in
        W8V.concat
          [ W8V.fromList sig8,
            chunk ("IHDR", ihdr),
            chunk ("IDAT", zstream),
            chunk ("IEND", W8V.fromList []) ]
      end
  end

  (* ===================== detect / dispatch ===================== *)

  datatype format = PNM | BMP | TGA | PNG

  fun detect v =
    if W8V.length v >= 8
       andalso u8 (v, 0) = 0x89 andalso u8 (v, 1) = 0x50
       andalso u8 (v, 2) = 0x4E andalso u8 (v, 3) = 0x47
    then SOME PNG
    else if W8V.length v >= 2 andalso u8 (v, 0) = 0x42 andalso u8 (v, 1) = 0x4D
    then SOME BMP
    else if W8V.length v >= 2 andalso u8 (v, 0) = 0x50
            andalso (let val k = u8 (v, 1) in k >= 0x31 andalso k <= 0x36 end)
    then SOME PNM
    else if W8V.length v >= 18 andalso u8 (v, 1) = 0 andalso u8 (v, 2) = 2
    then SOME TGA
    else NONE

  fun decode v =
    case detect v of
      SOME PNG => decodePng v
    | SOME BMP => decodeBmp v
    | SOME PNM => decodePnm v
    | SOME TGA => decodeTga v
    | NONE => raise Image "decode: unrecognized format"
end
