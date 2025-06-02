structure R64 =
struct (* need this for fromLargeWord *) open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

val n = CommandLineArgs.parseInt "n" 1000000


val rand = Random.rand (15, 210)
val s = 100000000.0
val rs_orig = ArraySlice.full (Array.tabulate (n, fn _ =>
  let val r = Math.tan (Random.randReal rand - 0.5) * s
  in r
  end))

val rs = ArraySlice.full (Array.tabulate (n, fn i =>
  let
    val r = Seq.nth rs_orig i
  in
    case Random.randRange (0, 2) rand of
      0 => Real.fmt (StringCvt.FIX (SOME (Random.randRange (1, 3) rand))) r
    | 1 => Real.fmt (StringCvt.GEN (SOME (Random.randRange (5, 10) rand))) r
    | 2 => Real.fmt (StringCvt.SCI (SOME (Random.randRange (5, 10) rand))) r
    | _ => Real.fmt StringCvt.EXACT r
  end))


val rs_charseqs =
  Seq.map (fn s => Seq.tabulate (fn i => String.sub (s, i)) (String.size s)) rs
val offsets =
  ArraySlice.full (SeqBasis.scan 1000 op+ 0 (0, Seq.length rs_charseqs) (fn i =>
    Seq.length (Seq.nth rs_charseqs i)))
val chars = Seq.flatten rs_charseqs

fun nth i =
  let
    val lo = Seq.nth offsets i
    val hi = Seq.nth offsets (i + 1)
  in
    Seq.subseq chars (lo, hi - lo)
  end


val _ = print
  ("\n\
   \==============================================================\n\
   \testing Real.scan (Standard ML basis function)\n\
   \==============================================================\n")

val rs_from_string = Benchmark.run "Real.scan" (fn () =>
  Seq.tabulate
    (fn i =>
       let
         val lo = Seq.nth offsets i
         val hi = Seq.nth offsets (i + 1)
         fun reader i =
           if i >= hi then NONE else SOME (Seq.nth chars i, i + 1)
       in
         #1 (valOf (Real.scan reader lo))
       end) n)

val _ = print
  ("\n\
   \==============================================================\n\
   \testing Parse.parseReal (mpllib)\n\
   \==============================================================\n")

val rs_parse = Benchmark.run "Parse.parseReal" (fn () =>
  Seq.tabulate (fn i => valOf (Parse.parseReal (nth i))) n)

val all_correct = Seq.equal Real.== (rs_from_string, rs_parse)
val _ = print ("ALL CORRECT? " ^ (if all_correct then "yes" else "no") ^ "\n")


val _ = print
  ("\n\
   \==============================================================\n\
   \testing FastReal.from_chars_with_info\n\
   \==============================================================\n")

val (rs_from_chars, num_fast) = Benchmark.run "from_chars" (fn () =>
  let
    val results = ForkJoin.alloc n

    val num_fast = SeqBasis.reduce 5000 op+ 0 (0, n) (fn i =>
      let
        val lo = Seq.nth offsets i
        val hi = Seq.nth offsets (i + 1)

        val {result, fast_path, ...} = valOf
          (FR.from_chars_with_info {start = lo, stop = hi, get = Seq.nth chars})
      in
        Array.update (results, i, result);
        if fast_path then 1 else 0
      end)
  in
    (ArraySlice.full results, num_fast)
  end)

val _ = print ("NUM FAST " ^ Int.toString num_fast ^ "\n")

val all_correct = Seq.equal Real.== (rs_from_string, rs_from_chars)
val _ = print ("ALL CORRECT? " ^ (if all_correct then "yes" else "no") ^ "\n")

(* val rtos = Real.fmt StringCvt.EXACT

val () = Util.for (0, n) (fn i =>
  let
    val r_from_char = Seq.nth rs_from_chars i
    val r_from_string = Seq.nth rs_from_string i
    val r_parse = Seq.nth rs_parse i
  in
    if
      Real.==
        ( r_from_char
        , r_from_string
        ) (* andalso Real.== (r_from_char, r_parse) *)
    then
      ()
    else
      print
        ("Found: " ^ Seq.nth rs i ^ " : " ^ rtos r_from_char ^ " "
         ^ rtos r_from_string ^ " " ^ rtos r_parse ^ "\n")
  end) *)
