structure SIMDParse =
struct
   val neon_parse = _import "parse_8_neon_u64" : char array * int -> Word64.word;
end

(* val tests = [
  "123456",
  "12345678901234567890"
] *)

(* fun test () = 
    let
        val _ = print "Testing digit parse\n"
        val _ = List.app (fn s => print ("Testing: " ^ s ^ "\n")) tests
        val _ = List.app (fn s => print ("  Parsed: " ^ Real.fmt StringCvt.DEC (neon_parse s 0) ^ "\n")) tests
    in
        print "Done\n"
    end *)

(* val _ = test () *)