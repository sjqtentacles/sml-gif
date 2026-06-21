(* test_structure.sml -- GIF89a byte-level structure assertions. *)

structure StructureTests =
struct
  open Support

  fun vecToString v =
    CharVector.tabulate (Word8Vector.length v, fn i => Char.chr (byteAt (v, i)))

  fun vecEq (a, b) =
    Word8Vector.length a = Word8Vector.length b
    andalso
    let
      fun lp i =
        if i >= Word8Vector.length a then true
        else if Word8Vector.sub (a, i) = Word8Vector.sub (b, i) then lp (i + 1)
        else false
    in lp 0 end

  (* count block markers at the byte level (good enough for the tiny, control
     fixtures used here): a 0x21 0xF9 pair (GCE) and a 0x2C (image descriptor). *)
  fun countPairs (v, a, b) =
    let
      val n = Word8Vector.length v
      fun lp (i, acc) =
        if i >= n - 1 then acc
        else if byteAt (v, i) = a andalso byteAt (v, i + 1) = b
        then lp (i + 1, acc + 1) else lp (i + 1, acc)
    in lp (0, 0) end

  fun countByte (v, a) =
    let
      val n = Word8Vector.length v
      fun lp (i, acc) =
        if i >= n then acc
        else if byteAt (v, i) = a then lp (i + 1, acc + 1) else lp (i + 1, acc)
    in lp (0, 0) end

  fun run () =
    let
      val () = Harness.section "header / trailer / screen descriptor"
      val img = mkImage (4, 3, fn (x, y) =>
        if (x + y) mod 2 = 0 then (200, 30, 30) else (20, 40, 220))
      val bytes = G.encode { width = 4, height = 3
                           , frames = [{ image = img, delayCs = 10 }], loop = 0 }
      val s = vecToString bytes
      val () = Harness.checkString "magic is GIF89a" ("GIF89a", String.substring (s, 0, 6))
      val () = Harness.checkInt "screen width" (4, u16At (bytes, 6))
      val () = Harness.checkInt "screen height" (3, u16At (bytes, 8))
      val () = Harness.checkBool "global color table flag set"
                 (true, byteAt (bytes, 10) >= 128)
      val () = Harness.checkInt "trailer byte is 0x3B"
                 (0x3B, byteAt (bytes, Word8Vector.length bytes - 1))

      val () = Harness.section "looping extension + per-frame control blocks"
      val () = Harness.checkBool "NETSCAPE2.0 application extension present"
                 (true, String.isSubstring "NETSCAPE2.0" s)
      val () = Harness.checkInt "one graphic control extension per frame"
                 (1, countPairs (bytes, 0x21, 0xF9))
      val () = Harness.checkInt "decoder recovers single frame"
                 (1, length (#frames (decodeGif bytes)))

      val () = Harness.section "multi-frame structure"
      val f0 = mkImage (3, 2, fn _ => (10, 10, 10))
      val f1 = mkImage (3, 2, fn _ => (250, 250, 250))
      val f2 = mkImage (3, 2, fn (x, _) => if x = 0 then (5, 200, 5) else (5, 5, 200))
      val anim = G.encode { width = 3, height = 2
                          , frames = [ { image = f0, delayCs = 5 }
                                     , { image = f1, delayCs = 7 }
                                     , { image = f2, delayCs = 9 } ]
                          , loop = 3 }
      val () = Harness.checkInt "three graphic control extensions"
                 (3, countPairs (anim, 0x21, 0xF9))
      val dec = decodeGif anim
      val () = Harness.checkInt "decoder sees three frames" (3, length (#frames dec))
      val () = Harness.checkString "version round-trips" ("GIF89a", #version dec)
      val () = Harness.checkBool "loop count recovered"
                 (true, #loop dec = SOME 3)

      val () = Harness.section "determinism"
      val again = G.encode { width = 4, height = 3
                           , frames = [{ image = img, delayCs = 10 }], loop = 0 }
      val () = Harness.checkBool "encode is byte-deterministic" (true, vecEq (bytes, again))

      val () = Harness.section "validation"
      val () = Harness.checkRaises "empty frame list raises"
                 (fn () => G.encode { width = 4, height = 3, frames = [], loop = 0 })
      val () = Harness.checkRaises "size mismatch raises"
                 (fn () => G.encode { width = 5, height = 3
                                    , frames = [{ image = img, delayCs = 0 }], loop = 0 })
      val () = Harness.checkRaises "non-positive dimension raises"
                 (fn () => G.encode { width = 0, height = 3
                                    , frames = [{ image = img, delayCs = 0 }], loop = 0 })
    in
      ()
    end
end
