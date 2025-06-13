structure Util =
struct

  (* // from numerical recipes
     * uint64_t hash64(uint64_t u)
     * {
     *   uint64_t v = u * 3935559000370003845ul + 2691343689449507681ul;
     *   v ^= v >> 21;
     *   v ^= v << 37;
     *   v ^= v >>  4;
     *   v *= 4768777513237032717ul;
     *   v ^= v << 20;
     *   v ^= v >> 41;
     *   v ^= v <<  5;
     *   return v;
     * }
     *)

  fun hash64 u =
    let
      open Word64
      infix 2 >> << xorb andb
      val v = u * 0w3935559000370003845 + 0w2691343689449507681
      val v = v xorb (v >> 0w21)
      val v = v xorb (v << 0w37)
      val v = v xorb (v >> 0w4)
      val v = v * 0w4768777513237032717
      val v = v xorb (v << 0w20)
      val v = v xorb (v >> 0w41)
      val v = v xorb (v << 0w5)
    in
      v
    end

  (* uint32_t hash32(uint32_t a) {
   *   a = (a+0x7ed55d16) + (a<<12);
   *   a = (a^0xc761c23c) ^ (a>>19);
   *   a = (a+0x165667b1) + (a<<5);
   *   a = (a+0xd3a2646c) ^ (a<<9);
   *   a = (a+0xfd7046c5) + (a<<3);
   *   a = (a^0xb55a4f09) ^ (a>>16);
   *   return a;
   * }
   *)

  fun hash32 a =
    let
      open Word32
      infix 2 >> << xorb
      val a = (a + 0wx7ed55d16) + (a << 0w12)
      val a = (a xorb 0wxc761c23c) xorb (a >> 0w19)
      val a = (a + 0wx165667b1) + (a << 0w5)
      val a = (a + 0wxd3a2646c) xorb (a << 0w9)
      val a = (a + 0wxfd7046c5) + (a << 0w3)
      val a = (a xorb 0wxb55a4f09) xorb (a >> 0w16)
    in
      a
    end

  (* uint32_t hash32_2(uint32_t a) {
   *   uint32_t z = (a + 0x6D2B79F5UL);
   *   z = (z ^ (z >> 15)) * (z | 1UL);
   *   z ^= z + (z ^ (z >> 7)) * (z | 61UL);
   *   return z ^ (z >> 14);
   * }
   *)

  fun hash32_2 a =
    let
      open Word32
      infix 2 >> << xorb orb
      val z = (a + 0wx6D2B79F5)
      val z = (z xorb (z >> 0w15)) * (z orb 0w1)
      val z = z xorb (z + (z xorb (z >> 0w7)) * (z orb 0w61))
    in
      z xorb (z >> 0w14)
    end

  (* inline uint32_t hash32_3(uint32_t a) {
   *   uint32_t z = a + 0x9e3779b9;
   *   z ^= z >> 15; // 16 for murmur3
   *   z *= 0x85ebca6b;
   *   z ^= z >> 13;
   *   z *= 0xc2b2ae3d; // 0xc2b2ae35 for murmur3
   *   return z ^= z >> 16;
   * }
   *)

  fun hash32_3 a =
    let
      open Word32
      infix 2 >> << xorb orb
      val z = a + 0wx9e3779b9
      val z = z xorb (z >> 0w15) (* 16 for murmur3 *)
      val z = z * 0wx85ebca6b
      val z = z xorb (z >> 0w13)
      val z = z * 0wxc2b2ae3d (* 0wxc2b2ae35 for murmur3 *)
      val z = z xorb (z >> 0w16)
    in
      z
    end

  (* // a slightly cheaper, but possibly not as good version
   * // based on splitmix64
   * inline uint64_t hash64_2(uint64_t x) {
   *   x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
   *   x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
   *   x = x ^ (x >> 31);
   *   return x;
   * }
   *)

  fun hash64_2 x =
    let
      open Word64
      infix 2 >> << xorb orb
      val x = (x xorb (x >> 0w30)) * 0wxbf58476d1ce4e5b9
      val x = (x xorb (x >> 0w27)) * 0wx94d049bb133111eb
      val x = x xorb (x >> 0w31)
    in
      x
    end

  (* This chooses which hash function to use for generic integers, since
   * integers are configurable at compile time. *)
  val hash: int -> int =
    case Int.precision of
      NONE => (Word64.toInt o hash64 o Word64.fromInt)
    | SOME 32 => (Word32.toIntX o hash32 o Word32.fromInt)
    | SOME 64 => (Word64.toIntX o hash64 o Word64.fromInt)
    | SOME p =>
        (fn x =>
           let
             val wp1 = Word.fromInt (p - 1)
             open Word64
             infix 2 >> << andb
             val v = hash64 (fromInt x)
             val v = v andb ((0w1 << wp1) - 0w1)
           in
             toInt v
           end)

end
