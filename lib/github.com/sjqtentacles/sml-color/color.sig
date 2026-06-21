(* color.sig

   Pure color-space types and conversions. All channel values are reals in
   [0, 1] unless stated otherwise; hue is in degrees [0, 360). Functions are
   total and deterministic, byte-identical across MLton and Poly/ML. *)

signature COLOR =
sig
  (* Channels in [0,1]. *)
  type rgb  = { r : real, g : real, b : real }
  type rgba = { r : real, g : real, b : real, a : real }
  (* h in [0,360), s,v,l in [0,1]. *)
  type hsv  = { h : real, s : real, v : real }
  type hsl  = { h : real, s : real, l : real }

  (* Clamp every channel into its valid range (hue is wrapped mod 360). *)
  val clampRgb  : rgb  -> rgb
  val clampRgba : rgba -> rgba

  val rgbToRgba : real -> rgb -> rgba          (* supply alpha *)
  val rgbaToRgb : rgba -> rgb                  (* drop alpha *)

  (* HSV / HSL conversions. Achromatic inputs (s = 0) yield hue 0. *)
  val rgbToHsv : rgb -> hsv
  val hsvToRgb : hsv -> rgb
  val rgbToHsl : rgb -> hsl
  val hslToRgb : hsl -> rgb

  (* sRGB <-> linear transfer functions (per-channel), endpoints exact. *)
  val srgbToLinear : real -> real
  val linearToSrgb : real -> real
  val rgbToLinear  : rgb -> rgb
  val rgbToSrgb    : rgb -> rgb

  (* 32-bit packing, channel order 0xRRGGBBAA (R is the most significant byte).
     pack clamps to [0,1] and rounds to nearest. *)
  val pack   : rgba -> Word32.word
  val unpack : Word32.word -> rgba

  (* Hex strings. Parses "#rgb", "#rgba", "#rrggbb", "#rrggbbaa" (leading '#'
     optional, case-insensitive); returns NONE on malformed input. *)
  val fromHex : string -> rgba option
  val toHex   : rgba -> string                 (* canonical lowercase "#rrggbbaa" *)
  val toHexRgb : rgb -> string                 (* "#rrggbb" *)

  (* Interpolation. lerp is unclamped; mix = lerp with t clamped to [0,1]. *)
  val lerp : rgba * rgba * real -> rgba
  val mix  : rgba * rgba * real -> rgba

  val approx : real -> rgba * rgba -> bool
  val approxRgb : real -> rgb * rgb -> bool
end
