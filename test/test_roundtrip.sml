(* test_roundtrip.sml -- encode then decode, verifying exact pixel recovery for
   palettes of <= 256 colors (the lossless path), plus delay/loop metadata. *)

structure RoundtripTests =
struct
  open Support

  (* count distinct RGB colors in an image *)
  fun distinctColors (img : Image.image) =
    let
      val seen = ref ([] : (int * int * int) list)
      val n = #width img * #height img
      fun lp p =
        if p >= n then ()
        else
          let val c = pixel img (p mod #width img, p div #width img)
          in if List.exists (fn x => x = c) (!seen) then () else seen := c :: !seen;
             lp (p + 1)
          end
    in lp 0; length (!seen) end

  fun roundtripOne (name, img, delayCs) =
    let
      val w = #width img and h = #height img
      val bytes = G.encode { width = w, height = h
                           , frames = [{ image = img, delayCs = delayCs }], loop = 0 }
      val dec = decodeGif bytes
    in
      Harness.checkInt (name ^ ": width") (w, #width dec);
      Harness.checkInt (name ^ ": height") (h, #height dec);
      Harness.checkInt (name ^ ": one frame") (1, length (#frames dec));
      case #frames dec of
          [{ delayCs = d, image = di }] =>
            ( Harness.checkInt (name ^ ": delay") (delayCs, d)
            ; Harness.checkBool (name ^ ": pixels exact") (true, sameImage (img, di)) )
        | _ => Harness.check (name ^ ": shape") false
    end

  fun run () =
    let
      val () = Harness.section "single-frame round trips (exact palette)"

      (* 1x1 edge case *)
      val () = roundtripOne ("1x1", mkImage (1, 1, fn _ => (123, 45, 67)), 0)

      (* two colors *)
      val () = roundtripOne ("2-color checker",
                 mkImage (5, 4, fn (x, y) =>
                   if (x + y) mod 2 = 0 then (0, 0, 0) else (255, 255, 255)), 4)

      (* a handful of colors *)
      val palette = Vector.fromList
                     [ (10, 20, 30), (200, 10, 10), (10, 200, 10), (10, 10, 200)
                     , (250, 250, 0), (0, 250, 250), (250, 0, 250), (128, 128, 128) ]
      val () = roundtripOne ("8-color pattern",
                 mkImage (16, 16, fn (x, y) =>
                   Vector.sub (palette, (x * 3 + y * 5) mod 8)), 12)

      (* gradient with exactly 256 distinct colors: a 256x1 grayscale ramp *)
      val ramp = mkImage (256, 1, fn (x, _) => (x, x, x))
      val () = Harness.checkInt "ramp has 256 colors" (256, distinctColors ramp)
      val () = roundtripOne ("256-gray ramp", ramp, 3)

      (* repetitive content that exercises longer LZW runs and code-width growth *)
      val () = roundtripOne ("repetitive bands",
                 mkImage (64, 64, fn (x, y) =>
                   let val b = (x div 4 + y div 4) mod 6
                   in Vector.sub (palette, b) end), 2)

      val () = Harness.section "animation metadata round trip"
      val fa = mkImage (8, 8, fn (x, _) => if x < 4 then (255, 0, 0) else (0, 0, 255))
      val fb = mkImage (8, 8, fn (_, y) => if y < 4 then (0, 255, 0) else (255, 255, 0))
      val bytes = G.encode { width = 8, height = 8
                           , frames = [ { image = fa, delayCs = 25 }
                                      , { image = fb, delayCs = 50 } ]
                           , loop = 7 }
      val dec = decodeGif bytes
      val () = Harness.checkInt "two frames" (2, length (#frames dec))
      val () = Harness.checkBool "loop = 7" (true, #loop dec = SOME 7)
      val () =
        case #frames dec of
            [a, b] =>
              ( Harness.checkInt "frame A delay" (25, #delayCs a)
              ; Harness.checkInt "frame B delay" (50, #delayCs b)
              ; Harness.checkBool "frame A pixels" (true, sameImage (fa, #image a))
              ; Harness.checkBool "frame B pixels" (true, sameImage (fb, #image b)) )
          | _ => Harness.check "two-frame shape" false
    in
      ()
    end
end
