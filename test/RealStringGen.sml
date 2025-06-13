structure RealStringGen :>
sig
  type seed

  (* This is the grammar from the Standard ML basis library:
   *   <real_str> ::= [+~-]?(<infnan>|<value>)
   *   <infnan> ::= (inf|infinity|nan)   (* case insensitive *)
   *   <value> ::= (<digs>.<digs>?|.<digs>)(e|E)[+~-]?<digs>?
   *   <digs> ::= [0-9]+
   *
   * However, this is incorrect:
   *   - The (e|E) part is optional and only parsed if there is at least one
   *     digit that follows it
   *   - In the front part, before (e|E), it is possible to have no dot
   *   - If there is a dot, there must be at least one decimal digit after
   *     the dot. (We call this the "trailing dot". There is a parameter below
   *     to allow for trailing dots, if desired. By default, trailing dots
   *     are not generated.)
   *)

  val seed_from_int: int -> seed
  val split_seed: seed -> (int -> seed) * seed

  val gen: seed -> string * seed
  val gen_with_params: {allow_trailing_dot: bool} -> seed -> string * seed

end =
struct

  type seed = int

  fun seed_from_int x = Util.hash x

  local val d = 1000000000
  in
    fun gen_real seed =
      let val seed' = Util.hash seed
      in (Real.fromInt (seed' mod d) / Real.fromInt d, seed')
      end
  end

  fun gen_int seed =
    let val seed' = Util.hash seed
    in (seed', seed')
    end

  fun split_seed seed =
    let
      val seed' = Util.hash seed
      fun get_seed i =
        Util.hash (seed' + i + 1)
    in
      (get_seed, seed')
    end


  fun gen_maybe_plus_minus seed =
    let
      val (r, seed) = gen_real seed
      val output =
        if r < 0.166667 then "+"
        else if r < 0.333333 then "-"
        else if r < 0.5 then "~"
        else ""
    in
      (output, seed)
    end


  fun gen_infnan seed =
    let
      val (r, seed) = gen_real seed
      val value =
        if r < 0.333333 then "inf"
        else if r < 0.666667 then "infinity"
        else "nan"
    in
      (value, seed)
    end


  fun gen_length desired_expected cap seed =
    let
      val p = 1.0 / Real.fromInt desired_expected
      fun loop i seed =
        if i >= cap then
          (i, seed)
        else
          let val (r, seed) = gen_real seed
          in if r < p then (i, seed) else loop (i + 1) seed
          end
    in
      loop 1 seed
    end


  fun gen_dig seed =
    let val (i, seed) = gen_int seed
    in (Char.chr ((i mod 10) + Char.ord #"0"), seed)
    end


  fun gen_digs desired_expected_length max_length seed =
    let
      val (len, seed) = gen_length desired_expected_length max_length seed
      val (get_seed, seed) = split_seed seed
      val digs = CharVector.tabulate (len, fn i => #1 (gen_dig (get_seed i)))
    in
      (digs, seed)
    end


  fun gen_digs_dot_maybe_digs (params as {allow_trailing_dot: bool})
    desired_expected_length max_length seed =
    let
      val (digs, seed) = gen_digs desired_expected_length max_length seed
      val (r, seed) = gen_real seed
    in
      if allow_trailing_dot andalso r < 0.5 then
        (digs ^ ".", seed)
      else
        let
          val (after_digs, seed) =
            gen_digs desired_expected_length max_length seed
        in
          (digs ^ "." ^ after_digs, seed)
        end
    end


  fun gen_dot_digs desired_expected_length max_length seed =
    let val (digs, seed) = gen_digs desired_expected_length max_length seed
    in ("." ^ digs, seed)
    end


  fun gen_exponent seed =
    let
      val (pm, seed) = gen_maybe_plus_minus seed
      val (x, seed) = gen_int seed
      val x = x mod 11
      val (digs, seed) =
        if x >= 10 then gen_digs 2 3 seed
        else (Char.toString (Char.chr (x + Char.ord #"0")), seed)
    in
      ("e" ^ pm ^ digs, seed)
    end


  fun gen_real_value params seed =
    let
      val (r, seed) = gen_real seed
      val (front, seed) =
        if r < 0.5 then gen_digs_dot_maybe_digs params 5 10 seed
        else gen_dot_digs 10 15 seed

      val (r, seed) = gen_real seed
      val (back, seed) = if r < 0.5 then gen_exponent seed else ("", seed)
    in
      (front ^ back, seed)
    end


  fun gen_with_params params seed =
    let
      val (front, seed) = gen_maybe_plus_minus seed
      val (r, seed) = gen_real seed
      val (rest, seed) =
        if r < 0.1 then gen_infnan seed else gen_real_value params seed
    in
      (front ^ rest, seed)
    end

  fun gen seed =
    gen_with_params {allow_trailing_dot = false} seed

end
