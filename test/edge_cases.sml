structure R64 =
struct open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

fun compare_parsing test_str =
  let
    fun reader i = if i >= String.size test_str then NONE else SOME (String.sub (test_str, i), i + 1)
    val scan_result = Real.scan reader 0
    val fast_result = FR.from_string_with_info test_str
    fun print_result (prefix, result_opt) =
      case result_opt of
        NONE => print (prefix ^ "failed to parse\n")
      | SOME (r, n) => print (prefix ^ "result=" ^ Real.fmt StringCvt.EXACT r ^ 
                             ", num_chomped=" ^ Int.toString n ^ "\n")
  in
    print ("\nTest string: \"" ^ test_str ^ "\"\n");
    case (scan_result, fast_result) of
      (NONE, NONE) =>
        print "Both parsers failed to parse (expected for invalid input)\n"
    | (SOME (r_scan, stop_scan), NONE) =>
        (print_result ("Real.scan:    ", SOME (r_scan, stop_scan));
         print_result ("FastReal:     ", NONE);
         print "Results differ!\n")
    | (NONE, SOME {result = r_fast, num_chomped = stop_fast, ...}) =>
        (print_result ("Real.scan:    ", NONE);
         print_result ("FastReal:     ", SOME (r_fast, stop_fast));
         print "Results differ!\n")
    | (SOME (r_scan, stop_scan), SOME {result = r_fast, num_chomped = stop_fast, ...}) =>
        (print_result ("Real.scan:    ", SOME (r_scan, stop_scan));
         print_result ("FastReal:     ", SOME (r_fast, stop_fast));
         if Real.== (r_scan, r_fast) andalso stop_scan = stop_fast then
           print "Results match\n"
         else
           print "Results differ!\n")
  end

val edge_cases = [
  "123.456e+",
  "123.456e-",
  "123.456E",
  
  ".123",
  (* "123." *) (* ! Trailing dot related to Issue #3 *)
  "0.0",
  "-0.0",
  "+0.0",
  
  "1e0",
  "1e+0",
  "1e-0",
  "1e+",
  "1e-",
  
  "1e308",
  "1e-308",
  
  "inf",
  "+inf",
  "-inf",
  (* "nan", *) (* ! Will always return false *)
  "infity",
  
  "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
  "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
  
  "123.456.789",
  
  "--123.456",
  "++123.456",
  "-+123.456",
  
  " 123.456",
  "123.456 ",
  "\t123.456"
]

val _ = print "\n=== Testing Edge Cases ===\n"
val _ = List.app compare_parsing edge_cases

(* val () = print "SANITY CHECK: (nan, nan)\n"
val nan = Real.fromString "nan"
val _ = if Real.== (valOf nan, valOf nan) then print "No\n" else print "Yes\n" *)