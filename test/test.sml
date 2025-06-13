structure R64 =
struct (* need this for fromLargeWord *) open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

val n = CommandLineArgs.parseInt "n" 1000000
val seed = CommandLineArgs.parseInt "seed" 15210

val rs =
  Seq.tabulate
    (fn i => #1 (RealStringGen.gen (RealStringGen.seed_from_int (seed + i)))) n

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


fun report_errors results =
  let
    fun cmp (x, y) =
      (Real.isNan x andalso Real.isNan y) orelse Real.== (x, y)

    fun combine (a, b) =
      case a of
        NONE => b
      | _ => a

    val error =
      if Seq.length results = n then
        SeqBasis.reduce 1000 combine NONE (0, n) (fn i =>
          if cmp (Seq.nth rs_from_string i, Seq.nth results i) then
            NONE
          else
            SOME
              ("MISMATCH\n  input: " ^ Seq.nth rs i ^ "\n  expected: "
               ^ Real.fmt StringCvt.EXACT (Seq.nth rs_from_string i)
               ^ "\n  but got: " ^ Real.fmt StringCvt.EXACT (Seq.nth results i)
               ^ "\n"))
      else
        SOME "overall length mismatch\n"
  in
    case error of
      NONE => print ("ALL CORRECT? yes\n")
    | SOME e => print ("ALL CORRECT? no!\n" ^ e)
  end


(* val _ = print
  ("\n\
   \==============================================================\n\
   \testing Parse.parseReal (mpllib)\n\
   \==============================================================\n")

val rs_parse =
  Benchmark.run "Parse.parseReal" (fn () =>
    Seq.tabulate (fn i => valOf (Parse.parseReal (nth i))) n)
  handle e => (print ("ERROR: " ^ exnMessage e ^ "\n"); Seq.empty ())

val () = report_errors rs_parse *)


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
        if fast_path then 1 else ( (*print (Seq.nth rs i ^ "\n");*)0)
      end)
  in
    (ArraySlice.full results, num_fast)
  end)

val _ = print ("NUM FAST " ^ Int.toString num_fast ^ "\n")
val () = report_errors rs_from_chars
