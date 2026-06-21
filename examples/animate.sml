(* sml-gif demo: a looping, tween-style plasma animation.

   Everything is integer arithmetic (a triangle-wave "pseudo sine" plus integer
   colormap interpolation), so the frames -- and therefore the encoded GIF -- are
   byte-identical under MLton and Poly/ML. Each pixel's value comes from a fixed
   256-entry cyclic palette, so a frame has <= 256 distinct colors and the GIF
   encoder takes its lossless exact-palette path.

   Writes:
     assets/wave.gif        the animation
     assets/wave_frame.png  a single still frame (for inline display) *)

val width = 160
val height = 100
val frames = 32        (* divides 256 so the phase loops back seamlessly *)
val delayCs = 6        (* ~16 fps *)

(* ---- integer triangle wave, period 256, output 0..255 ---- *)
fun wrap (t, p) = let val m = t mod p in if m < 0 then m + p else m end
fun tri t =
  let val p = wrap (t, 256)
  in if p < 128 then p * 2 else (256 - p) * 2 - 2 end

(* integer sqrt for a radial term *)
fun isqrt n =
  let
    fun lp (x, y) = if y < x then lp ((x + n div x) div 2, x) else x
  in if n <= 0 then 0 else lp (n, n + 1) end

(* ---- cyclic colormap: 0..255 -> (r,g,b) ---- *)
val stops =
  Vector.fromList
    [ (  0,  30,  90) , ( 20, 140, 200) , ( 40, 200, 140)
    , (220, 220,  60) , (230,  90,  40) , (160,  40, 130)
    , ( 60,  30, 110) ]      (* loops back toward the first *)
val nStops = Vector.length stops

fun lerp (a, b, num, den) = a + (b - a) * num div den

fun colormap idx =
  let
    val seg = nStops             (* cyclic: nStops segments around the wheel *)
    val span = 256
    val pos = idx * seg          (* scaled position *)
    val s = pos div span
    val frac = pos mod span
    val (r0, g0, b0) = Vector.sub (stops, s mod nStops)
    val (r1, g1, b1) = Vector.sub (stops, (s + 1) mod nStops)
  in
    ( lerp (r0, r1, frac, span)
    , lerp (g0, g1, frac, span)
    , lerp (b0, b1, frac, span) )
  end

val cx = width div 2
val cy = height div 2

fun valueAt (x, y, phase) =
  let
    val dx = x - cx and dy = y - cy
    val rad = isqrt (dx * dx + dy * dy)
    val a = tri (x * 2 + phase)
    val b = tri (y * 2 - phase)
    val c = tri ((x + y) + phase)
    val d = tri (rad * 2 - phase * 2)
    val v = (a + b + c + d) div 4
    (* posterize into contour bands: large flat regions compress well under LZW
       and give a clean, deliberate plasma look with <= ~37 distinct colors *)
  in
    (v div 7) * 7
  end

fun renderFrame i : Image.image =
  let
    val phase = i * (256 div frames)
    val data = Word8Array.array (4 * width * height, 0w0)
    fun lp p =
      if p >= width * height then ()
      else
        let
          val x = p mod width and y = p div width
          val v = valueAt (x, y, phase)
          val (r, g, b) = colormap v
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
    { width = width, height = height, data = Word8Array.vector data }
  end

val imgs = List.tabulate (frames, renderFrame)
val gifFrames = List.map (fn im => { image = im, delayCs = delayCs }) imgs

val gifBytes = Gif.encode { width = width, height = height
                          , frames = gifFrames, loop = 0 }

val () =
  let val os = BinIO.openOut "assets/wave.gif"
  in BinIO.output (os, gifBytes); BinIO.closeOut os end

val () =
  let
    val still = renderFrame (frames div 3)
    val os = BinIO.openOut "assets/wave_frame.png"
  in BinIO.output (os, Image.encodePng still); BinIO.closeOut os end

val () =
  print ("wrote assets/wave.gif (" ^ Int.toString (Word8Vector.length gifBytes)
         ^ " bytes, " ^ Int.toString frames ^ " frames "
         ^ Int.toString width ^ "x" ^ Int.toString height ^ ")\n"
         ^ "wrote assets/wave_frame.png\n")
