(* gif.sig

   Pure-Standard-ML animated GIF (GIF89a) encoder.

   Frames are `sml-image` RGBA images. The encoder builds a single global color
   table by median-cut quantization (or an exact palette when a sequence uses
   <= 256 distinct colors), LZW-compresses each frame's indices, and emits a
   GIF89a byte stream with a NETSCAPE2.0 looping extension.

   Output is total and deterministic: byte-identical across MLton and Poly/ML.
   Alpha is ignored (GIF is opaque-RGB with at most a 1-bit transparent index,
   which this encoder does not emit); fully-transparent input is treated as its
   RGB value. Malformed input (no frames, non-positive or mismatched
   dimensions, out-of-range delay/loop) raises `Gif`. *)

signature GIF =
sig
  exception Gif of string

  (* One animation frame: an RGBA image plus its on-screen delay, in
     centiseconds (1 cs = 1/100 s), matching the GIF Graphic Control Extension. *)
  type frame = { image : Image.image, delayCs : int }

  (* Encode an animation to a GIF89a byte stream.
       width, height : logical screen size; every frame image must match.
       frames        : at least one frame, played in order.
       loop          : NETSCAPE2.0 loop count; 0 means loop forever, n>0 means
                       play n additional times.
     Raises `Gif` on empty/invalid input. *)
  val encode : { width : int
               , height : int
               , frames : frame list
               , loop : int } -> Word8Vector.vector
end
