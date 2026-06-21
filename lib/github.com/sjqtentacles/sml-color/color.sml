(* color.sml

   Implementation of COLOR. Channels are reals in [0,1]; hue in degrees. *)

structure Color :> COLOR =
struct
  type rgb  = { r : real, g : real, b : real }
  type rgba = { r : real, g : real, b : real, a : real }
  type hsv  = { h : real, s : real, v : real }
  type hsl  = { h : real, s : real, l : real }

  fun clamp01 (x : real) = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x
  fun wrapHue h =
    let val m = Real.rem (h, 360.0)
    in if m < 0.0 then m + 360.0 else m end

  fun clampRgb ({r,g,b} : rgb) : rgb =
    { r = clamp01 r, g = clamp01 g, b = clamp01 b }
  fun clampRgba ({r,g,b,a} : rgba) : rgba =
    { r = clamp01 r, g = clamp01 g, b = clamp01 b, a = clamp01 a }

  fun rgbToRgba a ({r,g,b} : rgb) : rgba = { r = r, g = g, b = b, a = a }
  fun rgbaToRgb ({r,g,b,a=_} : rgba) : rgb = { r = r, g = g, b = b }

  fun maxR (a, b) : real = if a > b then a else b
  fun minR (a, b) : real = if a < b then a else b

  (* Hue from rgb given max, min channels and chroma. *)
  fun hueOf (r, g, b, mx, c) =
    if Real.== (c, 0.0) then 0.0
    else
      let
        val h =
          if Real.== (mx, r) then Real.rem ((g - b) / c, 6.0)
          else if Real.== (mx, g) then (b - r) / c + 2.0
          else (r - g) / c + 4.0
      in
        wrapHue (h * 60.0)
      end

  fun rgbToHsv ({r,g,b} : rgb) : hsv =
    let
      val mx = maxR (r, maxR (g, b))
      val mn = minR (r, minR (g, b))
      val c = mx - mn
      val s = if Real.== (mx, 0.0) then 0.0 else c / mx
    in
      { h = hueOf (r, g, b, mx, c), s = s, v = mx }
    end

  fun hsvToRgb ({h,s,v} : hsv) : rgb =
    let
      val h = wrapHue h
      val c = v * s
      val hp = h / 60.0
      val x = c * (1.0 - Real.abs (Real.rem (hp, 2.0) - 1.0))
      val (r1, g1, b1) =
        if hp < 1.0 then (c, x, 0.0)
        else if hp < 2.0 then (x, c, 0.0)
        else if hp < 3.0 then (0.0, c, x)
        else if hp < 4.0 then (0.0, x, c)
        else if hp < 5.0 then (x, 0.0, c)
        else (c, 0.0, x)
      val m = v - c
    in
      { r = r1 + m, g = g1 + m, b = b1 + m }
    end

  fun rgbToHsl ({r,g,b} : rgb) : hsl =
    let
      val mx = maxR (r, maxR (g, b))
      val mn = minR (r, minR (g, b))
      val c = mx - mn
      val l = (mx + mn) / 2.0
      val s =
        if Real.== (c, 0.0) then 0.0
        else c / (1.0 - Real.abs (2.0 * l - 1.0))
    in
      { h = hueOf (r, g, b, mx, c), s = s, l = l }
    end

  fun hslToRgb ({h,s,l} : hsl) : rgb =
    let
      val h = wrapHue h
      val c = (1.0 - Real.abs (2.0 * l - 1.0)) * s
      val hp = h / 60.0
      val x = c * (1.0 - Real.abs (Real.rem (hp, 2.0) - 1.0))
      val (r1, g1, b1) =
        if hp < 1.0 then (c, x, 0.0)
        else if hp < 2.0 then (x, c, 0.0)
        else if hp < 3.0 then (0.0, c, x)
        else if hp < 4.0 then (0.0, x, c)
        else if hp < 5.0 then (x, 0.0, c)
        else (c, 0.0, x)
      val m = l - c / 2.0
    in
      { r = r1 + m, g = g1 + m, b = b1 + m }
    end

  fun srgbToLinear u =
    if u <= 0.04045 then u / 12.92
    else Math.pow ((u + 0.055) / 1.055, 2.4)
  fun linearToSrgb u =
    if u <= 0.0031308 then u * 12.92
    else 1.055 * Math.pow (u, 1.0 / 2.4) - 0.055

  fun rgbToLinear ({r,g,b} : rgb) : rgb =
    { r = srgbToLinear r, g = srgbToLinear g, b = srgbToLinear b }
  fun rgbToSrgb ({r,g,b} : rgb) : rgb =
    { r = linearToSrgb r, g = linearToSrgb g, b = linearToSrgb b }

  fun toByte x = Word32.fromInt (Real.round (clamp01 x * 255.0))

  fun pack ({r,g,b,a} : rgba) : Word32.word =
    let
      open Word32
    in
      orb (<< (toByte r, 0w24),
        orb (<< (toByte g, 0w16),
          orb (<< (toByte b, 0w8), toByte a)))
    end

  fun unpack (w : Word32.word) : rgba =
    let
      open Word32
      fun chan shift = Real.fromInt (toInt (andb (>> (w, shift), 0wxFF))) / 255.0
    in
      { r = chan 0w24, g = chan 0w16, b = chan 0w8, a = chan 0w0 }
    end

  (* --- hex --- *)
  fun hexDigit c =
    if Char.isDigit c then SOME (Char.ord c - Char.ord #"0")
    else
      let val lc = Char.toLower c
      in if lc >= #"a" andalso lc <= #"f"
         then SOME (Char.ord lc - Char.ord #"a" + 10)
         else NONE
      end

  fun hexPair (a, b) =
    case (hexDigit a, hexDigit b) of
        (SOME x, SOME y) => SOME (x * 16 + y)
      | _ => NONE

  fun byteToReal n = Real.fromInt n / 255.0

  fun fromHex s0 =
    let
      val s = if String.size s0 > 0 andalso String.sub (s0, 0) = #"#"
              then String.extract (s0, 1, NONE) else s0
      val cs = String.explode s
      fun nib c = Option.map (fn d => d * 16 + d) (hexDigit c)
      fun mk (r, g, b, a) =
        Option.map (fn ((rr,gg),(bb,aa)) =>
          { r = byteToReal rr, g = byteToReal gg,
            b = byteToReal bb, a = byteToReal aa })
          (case (r, g, b, a) of
             (SOME r, SOME g, SOME b, SOME a) => SOME ((r,g),(b,a))
           | _ => NONE)
    in
      case cs of
          [r,g,b] => mk (nib r, nib g, nib b, SOME 255)
        | [r,g,b,a] => mk (nib r, nib g, nib b, nib a)
        | [r1,r2,g1,g2,b1,b2] =>
            mk (hexPair (r1,r2), hexPair (g1,g2), hexPair (b1,b2), SOME 255)
        | [r1,r2,g1,g2,b1,b2,a1,a2] =>
            mk (hexPair (r1,r2), hexPair (g1,g2), hexPair (b1,b2), hexPair (a1,a2))
        | _ => NONE
    end

  fun byteToHex n =
    let
      val s = String.map Char.toLower (Int.fmt StringCvt.HEX n)
    in
      (if String.size s < 2 then "0" ^ s else s)
    end

  fun toHex ({r,g,b,a} : rgba) =
    let
      fun bv x = Real.round (clamp01 x * 255.0)
    in
      "#" ^ byteToHex (bv r) ^ byteToHex (bv g)
          ^ byteToHex (bv b) ^ byteToHex (bv a)
    end
  fun toHexRgb ({r,g,b} : rgb) =
    let fun bv x = Real.round (clamp01 x * 255.0)
    in "#" ^ byteToHex (bv r) ^ byteToHex (bv g) ^ byteToHex (bv b) end

  fun lerp ({r=r1,g=g1,b=b1,a=a1} : rgba, {r=r2,g=g2,b=b2,a=a2} : rgba, t) : rgba =
    { r = r1 + (r2 - r1) * t, g = g1 + (g2 - g1) * t,
      b = b1 + (b2 - b1) * t, a = a1 + (a2 - a1) * t }
  fun mix (c1, c2, t) = lerp (c1, c2, clamp01 t)

  fun approx eps ({r=r1,g=g1,b=b1,a=a1} : rgba, {r=r2,g=g2,b=b2,a=a2} : rgba) =
    Real.abs (r1-r2) <= eps andalso Real.abs (g1-g2) <= eps
    andalso Real.abs (b1-b2) <= eps andalso Real.abs (a1-a2) <= eps
  fun approxRgb eps ({r=r1,g=g1,b=b1} : rgb, {r=r2,g=g2,b=b2} : rgb) =
    Real.abs (r1-r2) <= eps andalso Real.abs (g1-g2) <= eps
    andalso Real.abs (b1-b2) <= eps
end
