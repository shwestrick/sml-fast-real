structure SIMDParse =
struct
   val neon_parse = _import "parse_8_neon_u64" : char array * int -> Word64.word;
end