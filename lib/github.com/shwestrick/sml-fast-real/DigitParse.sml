structure DigitParse =
struct
  val parse = _import "neon_parse_digits" public:
        (string * int * Word64.word ref * int ref) -> unit;
  
  val simdAvailable = true; (* TODO: Do an env check to see if we can use SIMD *)

  fun parseString s =
    let
      val accRef = ref 0w0 : Word64.word ref
      val countRef = ref 0 : int ref
      val _ = parse (s, String.size s, accRef, countRef)
    in
      (!accRef, !countRef)
    end
end

val tests = [
  "123456",
  "12345678901234567890"
]

fun test () = 
    let
        val _ = print "Testing digit parse\n"
        val _ = List.app (fn s => print ("Testing: " ^ s ^ "\n")) tests
        val _ = List.app (fn s => print ("  Parsed: " ^ Word64.fmt StringCvt.DEC (#1 (DigitParse.parseString s)) ^ "\n")) tests
    in
        print "Done\n"
    end

(* val _ = test () *)