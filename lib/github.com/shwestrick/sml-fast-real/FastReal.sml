functor FastReal
  (R:
   sig
     include REAL
     val fromLargeWord: LargeWord.word -> real
   end):
sig
  (* implicitly defines a sequence of characters
   *   [ get(start), get(start+1), ..., get(stop-1) ]
   *)
  type chars = {start: int, stop: int, get: int -> char}

  type result_with_info = {result: R.real, num_chomped: int, fast_path: bool}

  val from_chars: chars -> R.real option
  val from_chars_with_info: chars -> result_with_info option

  val from_string: string -> R.real option
  val from_string_with_info: string -> result_with_info option

  val from_slice: char ArraySlice.slice -> R.real option
  val from_slice_with_info: char ArraySlice.slice -> result_with_info option
end =
struct

  type chars = {start: int, stop: int, get: int -> char}

  type result_with_info = {result: R.real, num_chomped: int, fast_path: bool}


  fun is_digit_char c =
    Char.>= (c, #"0") andalso Char.<= (c, #"9")

  fun digit_char_to_word64 c =
    Word64.- (Word64.fromInt (Char.ord c), 0w48) (* 48 = ord(#"0") *)

  fun push_digit_char acc c =
    Word64.* (0w10, acc) + digit_char_to_word64 c

  (* input string `lowercase_desired` should be all lowercase *)
  fun try_parse_string_case_insensitive lowercase_desired {start, stop, get} =
    let
      val n = String.size lowercase_desired
      fun loop i =
        if
          i = n
        then true
        else if
          Char.toLower (get (start + i)) = String.sub (lowercase_desired, i)
        then loop (i + 1)
        else false
    in
      stop - start >= n andalso loop 0
    end

  fun skip_whitespace {start, stop, get} =
    let
      fun loop i =
        if i >= stop then i
        else if Char.isSpace (get i) then loop (i + 1)
        else i
    in
      loop start
    end
  
  val itos = Int.toString
  val btos = fn true => "true" | false => "false"
  val wtos = Word64.fmt StringCvt.DEC

  fun chars_to_substring {start: int, stop: int, get: int -> char} i j = 
    let
      fun loop acc k = 
        if k >= j then String.implode (rev acc)
        else loop (get k :: acc) (k + 1)
    in
      loop [] i
    end

  (* read digits and accumulate into `acc`, continuing until we see a
   * non-digit char, or until we hit `stop`
   *)
  fun push_digit_chars (acc: Word64.word)
    {start: int, stop: int, get: int -> char} : (Word64.word * int) =
    let
      fun scalar_loop (acc, i) =
        if i >= stop then
          (acc, i)
        else
          let
            val c = get i
          in
            if is_digit_char c then scalar_loop (push_digit_char acc c, i + 1)
            else (acc, i)
          end
    in
      scalar_loop (acc, start)
    end




  val min_exponent_fast_path =
    case R.precision of
      53 => ~22 (* 64-bit double *)
    | 24 => ~10 (* 32-bit float *)
    | _ => 0 (* otherwise, not sure *)


  val max_exponent_fast_path =
    case R.precision of
      53 => 22 (* 64-bit double *)
    | 24 => 10 (* 32-bit float *)
    | _ => 0 (* otherwise, not sure *)


  val max_mantissa_fast_path = Word64.<< (0w1, Word.fromInt R.precision)


  fun rtor (x: real) : R.real =
    R.fromLarge IEEEReal.TO_NEAREST (Real.toLarge x)


  val exact_powers_of_ten: R.real vector = Vector.fromList
    [ rtor 1e0
    , rtor 1e1
    , rtor 1e2
    , rtor 1e3
    , rtor 1e4
    , rtor 1e5
    , rtor 1e6
    , rtor 1e7
    , rtor 1e8
    , rtor 1e9
    , rtor 1e10
    , rtor 1e11
    , rtor 1e12
    , rtor 1e13
    , rtor 1e14
    , rtor 1e15
    , rtor 1e16
    , rtor 1e17
    , rtor 1e18
    , rtor 1e19
    , rtor 1e20
    , rtor 1e21
    , rtor 1e22
    ]

  fun exact_power_of_ten e = Vector.sub (exact_powers_of_ten, e)

  (* why isn't there a Real.nan ??? *)
  val nan = R./ (rtor 0.0, rtor 0.0)
  val () =
    if R.isNan nan then
      ()
    else
      raise Fail
        "FastReal: fatal error: could not construct NaN. \
        \Please submit a bug report at \
        \https://github.com/shwestrick/sml-fast-real/issues"

  fun negify is_negative r =
    if is_negative then R.~ r else r

  fun maybe_parse_inf_or_nan {start, stop, get, i, is_negative} =
    let
      val c = Char.toLower (get i)
      val p = try_parse_string_case_insensitive
    in
      (* check for nan *)
      if p "nan" {start = i, stop = stop, get = get} then
        SOME {result = nan, num_chomped = i - start + 3, fast_path = true}

      (* check for inf or infinity *)
      else if p "inf" {start = i, stop = stop, get = get} then
        let
          val is_full_word = p "inity" {start = i + 3, stop = stop, get = get}
        in
          SOME
            { result = negify is_negative R.posInf
            , num_chomped = i - start + (if is_full_word then 8 else 3)
            , fast_path = true
            }
        end

      else
        NONE
    end

  (* locally used, only in from_chars_with_info_maybe_error, for early return *)
  exception FromCharsError

  (* ========================================================================
   * Main parsing function. Parsing moves forward by advancing `i`, the
   * current index. We accumulate digits into `mantissa: Word64.word`, and
   * keep track of the number of digits accumulated with `digit_count`. This
   * approach is capable of handling at most 19 decimal digits; if the
   * digit_count exceeds 19, then we need to fall back on a slow path.
   *)
  fun from_chars_with_info_maybe_error {start: int, stop: int, get: int -> char} =
    let
      val i = skip_whitespace {start = start, stop = stop, get = get}
      val () = if i >= stop then raise FromCharsError else ()

      (* [+~-] *)
      val (is_negative, i) =
        if get i = #"-" orelse get i = #"~" then (true, i + 1)
        else if get i = #"+" then (false, i + 1)
        else (false, i)

      (* [0-9]+? *)
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
              if get i = #"-" orelse get i = #"~" then (true, i + 1)
              else if get i = #"+" then (false, i + 1)
              else (false, i)

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

    (* val _ = print ("start " ^ itos start ^ "\n")
    val _ = print ("stop  " ^ itos stop ^ "\n")
    val _ = print ("i     " ^ itos i ^ "\n")
    val _ = print ("neg   " ^ btos is_negative ^ "\n")
    val _ = print ("dot   " ^ btos has_dot ^ "\n")
    val _ = print ("e     " ^ btos has_explicit_exponent ^ "\n")
    val _ = print ("edig  " ^ itos explicit_exponent_digit_count ^ "\n")
    val _ = print ("dig   " ^ itos digit_count ^ "\n")
    val _ = print ("mantissa " ^ wtos mantissa ^ "\n")
    val _ = print ("exponent " ^ itos exponent ^ "\n") *)
    in
      (* checking for fast path; not 100% sure about explicit_exponent_digit_count *)
      if
        digit_count > 0 andalso digit_count <= 19
        andalso explicit_exponent_digit_count <= 19
        andalso min_exponent_fast_path <= exponent
        andalso exponent <= max_exponent_fast_path
        andalso mantissa <= max_mantissa_fast_path
      then
        (* TODO: overheads of fromLargeWord? failure cases?
         * fromLargeWord seems to depend on rounding mode?
         * (use IEEEReal.getRoundingMode?)
         *)
        let
          val value = R.fromLargeWord (Word64.toLarge mantissa)
          val value =
            if exponent < 0 then R./ (value, exact_power_of_ten (~exponent))
            else R.* (value, exact_power_of_ten exponent)
          val value = if is_negative then R.~ value else value
        in
          SOME {result = value, num_chomped = i - start, fast_path = true}
        end

      else if
        digit_count = 0
      then
        (* If no mantissa digits (either before or after the dot, if any), then
         * it's possible we might still see "inf" or "nan" or similar. In this
         * case, the index `i` is correct, and we can attempt to parse these
         * cases here.
         *)
        if not has_explicit_exponent andalso i < stop then
          maybe_parse_inf_or_nan
            { start = start
            , stop = stop
            , get = get
            , i = i
            , is_negative = is_negative
            }
        else
          NONE

      (* fallback: slow path *)
      else
        let
          fun reader i =
            if i >= stop then NONE else SOME (get i, i + 1)
        in
          Option.map
            (fn (r, i') =>
               {result = r, num_chomped = i' - start, fast_path = false})
            (R.scan reader start)
        end

    end

  exception FromSliceError

  fun from_slice_with_info_maybe_error (slice : char ArraySlice.slice) =
  let
    val (arr, slice_start, slice_len) = ArraySlice.base slice
    
    val get = fn i => Array.sub (arr, slice_start + i)
    
    val start = 0
    val stop = slice_len
    
    val i = skip_whitespace {start = start, stop = stop, get = get}
    val () = if i >= stop then raise FromSliceError else ()

    val (is_negative, i) =
      if get i = #"-" orelse get i = #"~" then (true, i + 1)
      else if get i = #"+" then (false, i + 1)
      else (false, i)

    val (mantissa, i') =
      push_digit_chars 0w0 {start = i, stop = stop, get = get}
    val digit_count = i' - i
    val i = i'

    val (has_dot, i) =
      if i < stop andalso get i = #"." then (true, i + 1) else (false, i)
    
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
            if i < stop andalso (get i = #"-" orelse get i = #"~") then (true, i + 1)
            else if i < stop andalso get i = #"+" then (false, i + 1)
            else (false, i)

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
  in
    if
      digit_count > 0 andalso digit_count <= 19
      andalso explicit_exponent_digit_count <= 19
      andalso min_exponent_fast_path <= exponent
      andalso exponent <= max_exponent_fast_path
      andalso mantissa <= max_mantissa_fast_path
    then
      let
        val value = R.fromLargeWord (Word64.toLarge mantissa)
        val value =
          if exponent < 0 then R./ (value, exact_power_of_ten (~exponent))
          else R.* (value, exact_power_of_ten exponent)
        val value = if is_negative then R.~ value else value
      in
        SOME {result = value, num_chomped = i, fast_path = true}
      end
    else if
      digit_count = 0
    then
      if not has_explicit_exponent andalso i < stop then
        maybe_parse_inf_or_nan
          { start = start  (* now 0 *)
          , stop = stop    (* now slice_len *)
          , get = get
          , i = i
          , is_negative = is_negative
          }
      else
        NONE
    else
      let
        fun reader i =
          if i >= stop then NONE else SOME (get i, i + 1)
      in
        Option.map
          (fn (r, i') =>
             {result = r, num_chomped = i', fast_path = false})
          (R.scan reader 0)  (* start from 0 in relative coordinates *)
      end
  end

  fun from_slice_with_info_maybe_error_v2 (slice : char ArraySlice.slice) =
  let
    val (arr, slice_start, slice_len) = ArraySlice.base slice
    val slice_stop = slice_start + slice_len
    
    fun skip_whitespace_inline start =
      let
        fun loop i =
          if i >= slice_stop then i
          else if Char.isSpace (Array.sub (arr, i)) then loop (i + 1)
          else i
      in
        loop start
      end
    
    fun push_digit_chars_inline acc start_pos =
      let
        fun scalar_loop (acc, i) =
          if i >= slice_stop then
            (acc, i)
          else
            let
              val c = Array.sub (arr, i)
            in
              if is_digit_char c then 
                scalar_loop (push_digit_char acc c, i + 1)
              else (acc, i)
            end
      in
        scalar_loop (acc, start_pos)
      end
    
    fun try_parse_string_case_insensitive_inline lowercase_desired start_pos =
      let
        val n = String.size lowercase_desired
        fun loop i =
          if i = n then true
          else if start_pos + i >= slice_stop then false
          else if Char.toLower (Array.sub (arr, start_pos + i)) = String.sub (lowercase_desired, i)
          then loop (i + 1)
          else false
      in
        slice_stop - start_pos >= n andalso loop 0
      end
    
    fun maybe_parse_inf_or_nan_inline i is_negative =
      if i >= slice_stop then NONE
      else
        let
          val c = Char.toLower (Array.sub (arr, i))
        in
          if try_parse_string_case_insensitive_inline "nan" i then
            SOME {result = nan, num_chomped = i - slice_start + 3, fast_path = true}
          else if try_parse_string_case_insensitive_inline "inf" i then
            let
              val is_full_word = try_parse_string_case_insensitive_inline "inity" (i + 3)
            in
              SOME
                { result = negify is_negative R.posInf
                , num_chomped = i - slice_start + (if is_full_word then 8 else 3)
                , fast_path = true
                }
            end
          else
            NONE
        end
    
    val i = skip_whitespace_inline slice_start
    val () = if i >= slice_stop then raise FromSliceError else ()

    val (is_negative, i) =
      if Array.sub (arr, i) = #"-" orelse Array.sub (arr, i) = #"~" then (true, i + 1)
      else if Array.sub (arr, i) = #"+" then (false, i + 1)
      else (false, i)

    val (mantissa, i') = push_digit_chars_inline 0w0 i
    val digit_count = i' - i
    val i = i'

    val (has_dot, i) =
      if i < slice_stop andalso Array.sub (arr, i) = #"." then (true, i + 1) else (false, i)
    
    val (mantissa, i') = push_digit_chars_inline mantissa i
    val digit_count_past_dot = i' - i
    val exponent = ~digit_count_past_dot
    val digit_count = digit_count + digit_count_past_dot
    val i = i'

    val (has_explicit_exponent, explicit_exponent_digit_count, exponent, i) =
      if i >= slice_stop orelse (Array.sub (arr, i) <> #"e" andalso Array.sub (arr, i) <> #"E") then
        (false, 0, exponent, i)
      else
        let
          val i = i + 1

          val (explicit_exponent_is_negative, i) =
            if i < slice_stop andalso (Array.sub (arr, i) = #"-" orelse Array.sub (arr, i) = #"~") then (true, i + 1)
            else if i < slice_stop andalso Array.sub (arr, i) = #"+" then (false, i + 1)
            else (false, i)

          val (explicit_exponent_num, i') = push_digit_chars_inline 0w0 i
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
  in
    if
      digit_count > 0 andalso digit_count <= 19
      andalso explicit_exponent_digit_count <= 19
      andalso min_exponent_fast_path <= exponent
      andalso exponent <= max_exponent_fast_path
      andalso mantissa <= max_mantissa_fast_path
    then
      let
        val value = R.fromLargeWord (Word64.toLarge mantissa)
        val value =
          if exponent < 0 then R./ (value, exact_power_of_ten (~exponent))
          else R.* (value, exact_power_of_ten exponent)
        val value = if is_negative then R.~ value else value
      in
        SOME {result = value, num_chomped = i - slice_start, fast_path = true}
      end

    else if digit_count = 0 then
      if not has_explicit_exponent then
        maybe_parse_inf_or_nan_inline i is_negative
      else
        NONE

    else
      let
        fun reader abs_i =
          if abs_i >= slice_stop then NONE 
          else SOME (Array.sub (arr, abs_i), abs_i + 1)
      in
        Option.map
          (fn (r, final_abs_i) =>
             {result = r, num_chomped = final_abs_i - slice_start, fast_path = false})
          (R.scan reader slice_start)
      end
  end

  fun from_slice_with_info_simd (slice : char ArraySlice.slice) =
  let
    fun push_digit_chars_simd (acc: Word64.word) (arr: char array) (start_abs: int) (stop_abs: int) : (Word64.word * int) =
      let
        fun scalar_fallback (acc, i) =
          if i >= stop_abs then (acc, i)
          else
            let
              val c = Array.sub (arr, i)
            in
              if is_digit_char c then 
                scalar_fallback (push_digit_char acc c, i + 1)
              else (acc, i)
            end

        fun simd_accelerated_loop (acc, i) =
          if i + 8 <= stop_abs then
            let
              val digit_block = SIMDParse.neon_parse (arr, i)
            in
              if Word64.compare (digit_block, Word64.fromInt ~1) = EQUAL then
                scalar_fallback (acc, i)
              else
                let
                  val new_acc = Word64.* (acc, 0w100000000) + digit_block
                in
                  simd_accelerated_loop (new_acc, i + 8)
                end
            end
          else
            scalar_fallback (acc, i)
      in
        simd_accelerated_loop (acc, start_abs)
      end
    
    val (arr, slice_start, slice_len) = ArraySlice.base slice
    val slice_stop = slice_start + slice_len
    
    fun skip_whitespace_inline start =
      let
        fun loop i =
          if i >= slice_stop then i
          else if Char.isSpace (Array.sub (arr, i)) then loop (i + 1)
          else i
      in
        loop start
      end
    
    fun try_parse_string_case_insensitive_inline lowercase_desired start_pos =
      let
        val n = String.size lowercase_desired
        fun loop i =
          if i = n then true
          else if start_pos + i >= slice_stop then false
          else if Char.toLower (Array.sub (arr, start_pos + i)) = String.sub (lowercase_desired, i)
          then loop (i + 1)
          else false
      in
        slice_stop - start_pos >= n andalso loop 0
      end
    
    fun maybe_parse_inf_or_nan_inline i is_negative =
      if i >= slice_stop then NONE
      else
        let
          val c = Char.toLower (Array.sub (arr, i))
        in
          if try_parse_string_case_insensitive_inline "nan" i then
            SOME {result = nan, num_chomped = i - slice_start + 3, fast_path = true}
          else if try_parse_string_case_insensitive_inline "inf" i then
            let
              val is_full_word = try_parse_string_case_insensitive_inline "inity" (i + 3)
            in
              SOME
                { result = negify is_negative R.posInf
                , num_chomped = i - slice_start + (if is_full_word then 8 else 3)
                , fast_path = true
                }
            end
          else
            NONE
        end
    
    val i = skip_whitespace_inline slice_start
    val () = if i >= slice_stop then raise FromSliceError else ()

    val (is_negative, i) =
      if Array.sub (arr, i) = #"-" orelse Array.sub (arr, i) = #"~" then (true, i + 1)
      else if Array.sub (arr, i) = #"+" then (false, i + 1)
      else (false, i)

    val (mantissa, i') = push_digit_chars_simd 0w0 arr i slice_stop
    val digit_count = i' - i
    val i = i'

    val (has_dot, i) =
      if i < slice_stop andalso Array.sub (arr, i) = #"." then (true, i + 1) else (false, i)
    
    val (mantissa, i') = push_digit_chars_simd mantissa arr i slice_stop
    val digit_count_past_dot = i' - i
    val exponent = ~digit_count_past_dot
    val digit_count = digit_count + digit_count_past_dot
    val i = i'

    val (has_explicit_exponent, explicit_exponent_digit_count, exponent, i) =
      if i >= slice_stop orelse (Array.sub (arr, i) <> #"e" andalso Array.sub (arr, i) <> #"E") then
        (false, 0, exponent, i)
      else
        let
          val i = i + 1

          val (explicit_exponent_is_negative, i) =
            if i < slice_stop andalso (Array.sub (arr, i) = #"-" orelse Array.sub (arr, i) = #"~") then (true, i + 1)
            else if i < slice_stop andalso Array.sub (arr, i) = #"+" then (false, i + 1)
            else (false, i)

          val (explicit_exponent_num, i') = push_digit_chars_simd 0w0 arr i slice_stop
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
  in
    if
      digit_count > 0 andalso digit_count <= 19
      andalso explicit_exponent_digit_count <= 19
      andalso min_exponent_fast_path <= exponent
      andalso exponent <= max_exponent_fast_path
      andalso mantissa <= max_mantissa_fast_path
    then
      let
        val value = R.fromLargeWord (Word64.toLarge mantissa)
        val value =
          if exponent < 0 then R./ (value, exact_power_of_ten (~exponent))
          else R.* (value, exact_power_of_ten exponent)
        val value = if is_negative then R.~ value else value
      in
        SOME {result = value, num_chomped = i - slice_start, fast_path = true}
      end

    else if digit_count = 0 then
      if not has_explicit_exponent then
        maybe_parse_inf_or_nan_inline i is_negative
      else
        NONE

    else
      let
        fun reader abs_i =
          if abs_i >= slice_stop then NONE 
          else SOME (Array.sub (arr, abs_i), abs_i + 1)
      in
        Option.map
          (fn (r, final_abs_i) =>
             {result = r, num_chomped = final_abs_i - slice_start, fast_path = false})
          (R.scan reader slice_start)
      end
  end 


  fun from_slice_with_info (slice : char ArraySlice.slice) : result_with_info option =
    from_slice_with_info_simd slice
    handle FromSliceError => NONE

  fun from_slice xxx =
    Option.map #result (from_slice_with_info xxx)

  fun from_chars_with_info xxx =
    from_chars_with_info_maybe_error xxx
    handle FromCharsError => NONE

  fun from_chars xxx =
    Option.map #result (from_chars_with_info xxx)

  fun from_string_with_info s =
    from_chars_with_info
      {start = 0, stop = String.size s, get = fn i => String.sub (s, i)}

  fun from_string s =
    from_chars
      {start = 0, stop = String.size s, get = fn i => String.sub (s, i)}


end
