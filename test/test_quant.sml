(* test_quant.sml -- median-cut quantization (> 256 colors) and LZW behaviour
   under large, high-entropy inputs that force code-width growth and dictionary
   resets. The quantized path is necessarily lossy, so we bound the error and
   confirm the global color table never exceeds 256 entries; the <=256-color
   stress images still round-trip exactly. *)

structure QuantTests =
struct
  open Support

  fun distinctCount (img : Image.image) =
    let
      val seen = ref ([] : int list)
      val n = #width img * #height img
      fun key (r, g, b) = r * 65536 + g * 256 + b
      fun lp p cnt =
        if p >= n then cnt
        else
          let
            val (r, g, b) = pixel img (p mod #width img, p div #width img)
            val k = key (r, g, b)
          in
            if List.exists (fn x => x = k) (!seen) then lp (p + 1) cnt
            else (seen := k :: !seen; lp (p + 1) (cnt + 1))
          end
    in lp 0 0 end

  fun avgError (a : Image.image, b : Image.image) =
    let
      val n = #width a * #height a
      fun lp p acc =
        if p >= n then acc
        else
          let
            val (ar, ag, ab) = pixel a (p mod #width a, p div #width a)
            val (br, bg, bb) = pixel b (p mod #width a, p div #width a)
            val e = abs (ar - br) + abs (ag - bg) + abs (ab - bb)
          in lp (p + 1) (acc + e) end
    in real (lp 0 0) / real (3 * n) end

  fun run () =
    let
      val () = Harness.section "median-cut quantization (> 256 colors)"
      (* 40x40 two-axis gradient = up to 1600 distinct colors *)
      val grad = mkImage (40, 40, fn (x, y) =>
        (x * 6 mod 256, y * 6 mod 256, (x + y) * 3 mod 256))
      val nd = distinctCount grad
      val () = Harness.checkBool "gradient has > 256 colors" (true, nd > 256)
      val bytes = G.encode { width = 40, height = 40
                           , frames = [{ image = grad, delayCs = 0 }], loop = 0 }
      val dec = decodeGif bytes
      val () = Harness.checkInt "decoded width" (40, #width dec)
      val () = Harness.checkInt "decoded height" (40, #height dec)
      val () = Harness.checkBool "global color table <= 256" (true, #gctSize dec <= 256)
      val di = case #frames dec of [f] => #image f | _ => grad
      val () = Harness.checkBool "decoded palette <= 256 colors"
                 (true, distinctCount di <= 256)
      val err = avgError (grad, di)
      val () = Harness.checkBool "quantization error is small (avg/channel < 16)"
                 (true, err < 16.0)

      val () = Harness.section "LZW stress: width growth + dictionary reset"
      (* large image, <= 256 colors, high-entropy index pattern via an LCG so the
         LZW dictionary fills repeatedly and must reset; must still be exact. *)
      val w = 96 and h = 96
      val stress = mkImage (w, h, fn (x, y) =>
        let
          (* small-magnitude pseudo-noise (avoids Int overflow on 31-bit Int);
             high enough entropy to repeatedly fill and reset the LZW table *)
          val v = (x * 7 + y * 13 + x * y) mod 256
        in (v, (v * 2 + 17) mod 256, (v * 3 + 91) mod 256) end)
      val ndS = distinctCount stress
      val () = Harness.checkBool "stress palette <= 256" (true, ndS <= 256)
      val sb = G.encode { width = w, height = h
                        , frames = [{ image = stress, delayCs = 0 }], loop = 0 }
      val sd = decodeGif sb
      val () =
        case #frames sd of
            [f] => Harness.checkBool "stress round-trips exactly"
                     (true, sameImage (stress, #image f))
          | _ => Harness.check "stress shape" false

      val () = Harness.section "solid frames (degenerate single-color palette)"
      val solid = mkImage (10, 10, fn _ => (77, 88, 99))
      val solidB = G.encode { width = 10, height = 10
                            , frames = [{ image = solid, delayCs = 0 }], loop = 0 }
      val solidD = decodeGif solidB
      val () =
        case #frames solidD of
            [f] => Harness.checkBool "solid round-trips" (true, sameImage (solid, #image f))
          | _ => Harness.check "solid shape" false
    in
      ()
    end
end
