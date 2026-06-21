(* inflate.sig

   Pure-SML DEFLATE / zlib / gzip decompression plus CRC-32 and Adler-32
   checksums. Operates on `Word8Vector.vector`. Total and deterministic;
   byte-identical across MLton and Poly/ML.

   Malformed or truncated input raises `Inflate` with a message, rather than
   a generic pattern-match failure. *)

signature INFLATE =
sig
  exception Inflate of string

  (* --- checksums --- *)
  val crc32   : Word8Vector.vector -> Word32.word
  val adler32 : Word8Vector.vector -> Word32.word

  (* --- decompression --- *)

  (* Raw DEFLATE (RFC 1951): no header or trailer. *)
  val inflate : Word8Vector.vector -> Word8Vector.vector

  (* zlib (RFC 1950): 2-byte header + DEFLATE + Adler-32 trailer.
     The trailer checksum is verified; a mismatch raises Inflate. *)
  val zlib : Word8Vector.vector -> Word8Vector.vector

  (* gzip (RFC 1952): gzip header + DEFLATE + CRC-32 / ISIZE trailer.
     The trailer checksum is verified; a mismatch raises Inflate. *)
  val gunzip : Word8Vector.vector -> Word8Vector.vector
end
