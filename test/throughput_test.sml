structure R64 =
struct (* need this for fromLargeWord *) open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

val n = CommandLineArgs.parseInt "n" 1000000
val seed = CommandLineArgs.parseInt "seed" 15210

val rs =
  Seq.tabulate
    (fn i => #1 (RealStringGen.gen (RealStringGen.seed_from_int (seed + i)))) n

(* Sneak-peek of rs (which is a Seq of strings) *)
(* val () = print (Util.summarizeArraySlice 100 (fn r => r) rs ^ "\n") *)

val total_length = SeqBasis.reduce 1000 op+ 0 (0, Seq.length rs) (fn i =>
  String.size (Seq.nth rs i))
val _ = print ("num inputs:  " ^ Int.toString n ^ "\n")
val _ = print ("total chars: " ^ Int.toString total_length ^ "\n")

(* to test that the num_chomped values are correct, let's put some garbage
 * characters after every real string. We'll use any character except for
 * a digit.
 *)
fun gen_char_except_digit seed =
  let
    (* printable range (including the space character) is [32,127),
     * and we want to exclude the 10 digits.
     *)
    val x = 32 + Util.hash seed mod (127 - 32 - 10)
  in
    if x < Char.ord #"0" then Char.chr x else Char.chr (x + 10)
  end

val seed = Util.hash (Util.hash seed)

val afters =
  Seq.tabulate
    (fn i => Seq.tabulate (fn j => gen_char_except_digit (seed + 5 * i + j)) 5)
    n

fun interleave s t =
  if Seq.length s <> Seq.length t then
    raise Fail "interleave"
  else
    Seq.tabulate
      (fn i => if i mod 2 = 0 then Seq.nth s (i div 2) else Seq.nth t (i div 2))
      (2 * Seq.length s)

val rs_charseqs =
  interleave
    (Seq.map (fn s => Seq.tabulate (fn i => String.sub (s, i)) (String.size s))
       rs) afters

val offsets =
  ArraySlice.full (SeqBasis.scan 1000 op+ 0 (0, Seq.length rs_charseqs) (fn i =>
    Seq.length (Seq.nth rs_charseqs i)))
val chars: char ArraySlice.slice = Seq.flatten rs_charseqs

(* strip away Seq for more efficient indexing, if possible. *)
val (chars_array, chars_array_start, _) = ArraySlice.base chars
val () = if chars_array_start = 0 then () else raise Fail "whoops, not a slice"

fun nth i =
  let
    val lo = Seq.nth offsets (2 * i)
    val hi = Seq.nth offsets (2 * i + 1)
  in
    (lo, hi)
  end

val repeat = CommandLineArgs.parseInt "repeat" 5
val warmup = CommandLineArgs.parseReal "warmup" 0.0
val _ = print ("repeat " ^ Int.toString repeat ^ "\n")
val _ = print ("warmup " ^ Real.toString warmup ^ "\n")

fun go f =
  let
    val warmup = Time.fromReal warmup
    fun warmupLoop startTime =
      if Time.>= (Time.- (Time.now (), startTime), warmup) then
        () (* warmup done! *)
      else
        let val (_, tm) = Util.getTime f
        in print ("warmup_run " ^ Time.fmt 4 tm ^ "s\n"); warmupLoop startTime
        end

    val _ =
      if Time.<= (warmup, Time.zeroTime) then
        ()
      else
        ( print ("====== WARMUP ======\n")
        ; warmupLoop (Time.now ())
        ; print ("==== END WARMUP ====\n")
        )

    val (result, tms) = Benchmark.getTimes "time" repeat f

    val tmin = Time.toReal
      (List.foldl (fn (a, m) => if Time.< (a, m) then a else m) (List.hd tms)
         (List.tl tms))

    val peak_throughput = Real.fromInt total_length / tmin
    val peak_throughput_mbs = peak_throughput / 1e6
  in
    print
      ("peak throughput: "
       ^ Real.fmt (StringCvt.FIX (SOME 3)) peak_throughput_mbs ^ " MB/s\n");
    result
  end

val _ = print
  "\n\
   \==============================================================\n\
   \testing Real.scan (Standard ML basis function)\n\
   \==============================================================\n"

val x = go (fn _ =>
  (* the reduction
   * is not computing anything interesting; it's really just a big for-loop
   * to call Real.scan many times. We return something to make sure it's
   * not all optimized away.
   *)
  SeqBasis.reduce 1000 (fn (a, b) => b) 0.0 (0, n) (fn i =>
    let
      val (lo, hi) = nth i
      fun reader i =
        if i >= hi then NONE else SOME (Array.sub (chars_array, i), i + 1)
      val (r, stop) = valOf (Real.scan reader lo)
    in
      r
    end))

val () =
  if Real.isNan x then print ("ignore: " ^ Real.toString x ^ "\n") else ()

val _ = print
  "\n\
   \==============================================================\n\
   \testing FastReal.from_chars_with_info\n\
   \==============================================================\n"

val num_fast = go (fn _ =>
  SeqBasis.reduce 5000 op+ 0 (0, n) (fn i =>
    let
      val (lo, hi) = nth i

      val {result, fast_path, num_chomped} = valOf (FR.from_chars_with_info
        {start = lo, stop = hi, get = fn i => Array.sub (chars_array, i)})
    in
      if fast_path then 1 else 0
    end))

val _ = print ("NUM FAST " ^ Int.toString num_fast ^ "\n")
