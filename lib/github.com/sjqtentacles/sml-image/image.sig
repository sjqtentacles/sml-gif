(* image.sig

   Pure-SML raster image codecs over a single in-memory representation:
   8-bit RGBA, row-major, top-left origin, 4 bytes per pixel.

   Supported containers (encode + decode unless noted):
     - PPM/PGM  (Netpbm: P2/P3 ascii, P5/P6 binary)
     - BMP      (24/32-bit uncompressed, bottom-up and top-down)
     - TGA      (uncompressed truecolor, type 2)
     - PNG      (8-bit grayscale/GA/RGB/RGBA/palette; filters None/Sub/Up/
                 Average/Paeth; decode for all listed, encode as RGBA)

   All operations are total and deterministic, byte-identical across MLton and
   Poly/ML. Malformed input raises `Image`. *)

signature IMAGE =
sig
  exception Image of string

  (* RGBA8, row-major, top-left origin. `data` length must be 4*width*height. *)
  type image = { width : int, height : int, data : Word8Vector.vector }

  (* --- pixel access (no bounds surprises: out of range raises Image) --- *)
  type rgba8 = { r : Word8.word, g : Word8.word, b : Word8.word, a : Word8.word }
  val getPixel : image -> int * int -> rgba8
  val setPixel : image -> int * int -> rgba8 -> image   (* functional update *)
  val fill     : int * int -> rgba8 -> image            (* w,h -> solid image *)

  (* --- Netpbm --- *)
  val decodePnm : Word8Vector.vector -> image           (* P2/P3/P5/P6 *)
  val encodePpm : image -> Word8Vector.vector           (* binary P6 (drops alpha) *)
  val encodePgm : image -> Word8Vector.vector           (* binary P5 (luma) *)

  (* --- BMP --- *)
  val decodeBmp : Word8Vector.vector -> image
  val encodeBmp : image -> Word8Vector.vector           (* 32-bit top-down *)

  (* --- TGA --- *)
  val decodeTga : Word8Vector.vector -> image
  val encodeTga : image -> Word8Vector.vector           (* 32-bit truecolor *)

  (* --- PNG --- *)
  val decodePng : Word8Vector.vector -> image
  val encodePng : image -> Word8Vector.vector           (* 8-bit RGBA, filter None *)

  (* --- format-sniffing convenience --- *)
  datatype format = PNM | BMP | TGA | PNG
  val detect : Word8Vector.vector -> format option
  val decode : Word8Vector.vector -> image              (* dispatch on detect *)
end
