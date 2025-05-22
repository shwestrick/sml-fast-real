functor FastReal(R: REAL):
sig
  val from_chars: {start: int, stop: int, get: int -> char}
                  -> {result: R.real, num_chomped: int} option
end =
struct

  fun is_digit_char c =
    Char.>= (c, #"0") andalso Char.<= (c, #"9")

  fun digit_char_to_word64 c =
    Word64.- (Word64.fromInt (Char.ord c), 0w48) (* 48 = ord(#"0") *)

  fun push_digit_char acc c =
    Word64.* (0w10, acc) + digit_char_to_word64 c


  (* read digits and accumulate into `acc`, continuing until we see a
   * non-digit char, or until we hit `stop`
   *)
  fun push_digit_chars (acc: Word64.word)
    {start: int, stop: int, get: int -> char} : (Word64.word * int) =
    let
      fun loop (acc, i) =
        if i >= stop then
          (acc, i)
        else
          let
            val c = get i
          in
            if is_digit_char c then loop (push_digit_char acc c, i + 1)
            else (acc, i)
          end
    in
      loop (acc, start)
    end


  val itos = Int.toString
  val btos = fn true => "true" | false => "false"
  val wtos = Word64.fmt StringCvt.DEC


  fun from_chars {start: int, stop: int, get: int -> char} =
    let
      val () = print ("[FastReal.from_chars: WARNING: work in progress!]\n")

      (* Parsing moves forward by advancing `i`, the current index.
       * We accumulate digits into `mantissa: Word64.word`, and keep track
       * of the number of digits accumulated with `digit_count`. This
       * approach is capable of handling at most 19 decimal digits; if the
       * digit_count exceeds 19, then we need to fall back on a slow path.
       *)

      val i = start
      val (is_negative, i) = if get i = #"-" then (true, i + 1) else (false, i)

      val (mantissa, i') =
        push_digit_chars 0w0 {start = i, stop = stop, get = get}
      val digit_count = i' - i
      val i = i'

      val (has_dot, i) =
        if i < stop andalso get i = #"." then (true, i + 1) else (false, i)

      (* even if there is no dot, the following lines "do the right thing":
       * mantissa and digit_count stay unchanged, exponent set to 0, etc.
       *)
      val (mantissa, i') =
        push_digit_chars mantissa {start = i, stop = stop, get = get}
      val digit_count_past_dot = i' - i
      val exponent = ~digit_count_past_dot
      val digit_count = digit_count + digit_count_past_dot
      val i = i'

      val (has_explicit_exponent, explicit_exponent_digit_count, exponent, i) =
        if i >= stop orelse (get i <> #"e" andalso get i <> #"E") then
          (false, 0, exponent, i)
        else
          let
            val i = i + 1

            val (explicit_exponent_is_negative, i) =
              if get i = #"-" then (true, i + 1) else (false, i)

            val (explicit_exponent_num, i') =
              push_digit_chars 0w0 {start = i, stop = stop, get = get}
            val explicit_exponent_digit_count = i' - i
            val i = i'

            val explicit_exponent_num = Word64.toIntX explicit_exponent_num
            val explicit_exponent_num =
              if explicit_exponent_is_negative then ~explicit_exponent_num
              else explicit_exponent_num

            val exponent = exponent + explicit_exponent_num
          in
            (true, explicit_exponent_digit_count, exponent, i)
          end

      val _ = print ("start " ^ itos start ^ "\n")
      val _ = print ("stop  " ^ itos stop ^ "\n")
      val _ = print ("i     " ^ itos i ^ "\n")
      val _ = print ("neg   " ^ btos is_negative ^ "\n")
      val _ = print ("dot   " ^ btos has_dot ^ "\n")
      val _ = print ("e     " ^ btos has_explicit_exponent ^ "\n")
      val _ = print ("edig  " ^ itos explicit_exponent_digit_count ^ "\n")
      val _ = print ("dig   " ^ itos digit_count ^ "\n")
      val _ = print ("mantissa " ^ wtos mantissa ^ "\n")
      val _ = print ("exponent " ^ itos exponent ^ "\n")
    in
      NONE
    end

end
