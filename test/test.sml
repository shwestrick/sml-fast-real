structure R64 =
struct (* need this for fromLargeWord *) open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

val n = CommandLineArgs.parseInt "n" 1000000
val seed = CommandLineArgs.parseInt "seed" 15210

val rs =
  Seq.tabulate
    (fn i => #1 (RealStringGen.gen (RealStringGen.seed_from_int (seed + i)))) n

(* Sneak-peek of rs (which is a Seq of strings) *)
val () = print (Util.summarizeArraySlice 100 (fn r => r) rs ^ "\n")

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
val chars = Seq.flatten rs_charseqs

val () = print (Util.summarizeArraySlice 100 Char.toString chars ^ "\n")
val () = print (Util.summarizeArraySlice 10 Int.toString offsets ^ "\n")

fun nth i =
  let
    val lo = Seq.nth offsets (2 * i)
    val hi = Seq.nth offsets (2 * i + 1)
  in
    (lo, hi)
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
         val (lo, hi) = nth i

         fun reader i =
           if i >= hi then NONE else SOME (Seq.nth chars i, i + 1)
         val (r, stop) = valOf (Real.scan reader lo)
       in
          if stop <> hi then print ("MISMATCH: " ^ Real.fmt StringCvt.EXACT r ^ "\n") else ();
         {result = r, num_chomped = stop - lo}
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
          let
            val expected = Seq.nth rs_from_string i
            val got = Seq.nth results i
          in
            if cmp (#result expected, #result got) then
              NONE
            else
              SOME
                ("MISMATCH\n  input: " ^ Seq.nth rs i ^ "\n  expected: "
                 ^ Real.fmt StringCvt.EXACT (#result expected)
                 ^ " (num_chomped: " ^ Int.toString (#num_chomped expected)
                 ^ ")\n  but got: " ^ Real.fmt StringCvt.EXACT (#result got)
                 ^ " (num_chomped: " ^ Int.toString (#num_chomped got) ^ ")\n")
          end)
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
        val (lo, hi) = nth i

        val {result, fast_path, num_chomped} = valOf
          (FR.from_chars_with_info {start = lo, stop = hi, get = Seq.nth chars})
      in
        Array.update (results, i, {result = result, num_chomped = num_chomped});
        if fast_path then 1 else ( (*print (Seq.nth rs i ^ "\n");*)0)
      end)
  in
    (ArraySlice.full results, num_fast)
  end)

val _ = print ("NUM FAST " ^ Int.toString num_fast ^ "\n")
val () = report_errors rs_from_chars

(* Print result of results *)
val () = print (Util.summarizeArraySlice 100 (fn r => Real.fmt StringCvt.EXACT (#result r)) rs_from_chars ^ "\n")


val () = print "SANITY CHECK: 123.456e+\n";
val test_str = "123.456e+"
fun reader i = if i >= String.size test_str then NONE else SOME (String.sub (test_str, i), i + 1)
val (r, stop) = valOf (Real.scan reader 0)
val _ = print ("Real.scan num_chomped: " ^ Int.toString stop ^ "\n")
val {num_chomped, ...} = valOf (FR.from_chars_with_info 
  {start = 0, stop = String.size test_str, get = fn i => String.sub (test_str, i)})
val _ = print ("FastReal num_chomped: " ^ Int.toString num_chomped ^ "\n")