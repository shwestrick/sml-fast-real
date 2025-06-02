# sml-fast-real

:warning: work in progress :warning:

  * (Status as of 6/2/2025): Fast path appears to be working.
    Parse-back agrees with `Real.scan` on all inputs tested so far.
    Error cases are not fully handled yet; see issue #1.

Standard ML library for faster parsing of Reals (floats/doubles), heavily
inspired by [`fast_float`](https://github.com/fastfloat/fast_float).

Features a zero-allocation fast path that is as much as 8x faster than the
Basis implementations of `Real.scan` and `Real.fromString`. The slow
path currently falls back on `Real.scan`.

Compatible with the [`smlpkg`](https://github.com/diku-dk/smlpkg)
package manager.

## Library Sources

There is one source file, tested with both [MLton](https://github.com/mlton/mlton)
and [MaPLe](https://github.com/mpllang/mpl).

  * `lib/github.com/shwestrick/sml-fast-real/sources.mlb`

## Interface

The library defines a functor, [`FastReal`](lib/github.com/shwestrick/sml-fast-real/FastReal.sml),
which takes an implementation of Reals as input.

The input structure needs to also
define a function `fromLargeWord: LargeWord.word -> real` which rounds the
input value (interpreted as an unsigned integer) to the nearest representable
floating point value.

This function is used on the fast path; ideally, it
should have very low overhead and zero allocation.
In MLton, suitable functions are `MLton.Real32.fromLargeWord` and
`MLton.Real64.fromLargeWord`. See below for example usage.

```sml
functor FastReal
  (R:
   sig
     include REAL
     val fromLargeWord: LargeWord.word -> real
   end):
sig
  val from_chars_with_info:
    {start: int, stop: int, get: int -> char}
    -> {result: R.real, num_chomped: int, fast_path: bool} option

  (* the rest of these functions are just wrappers around from_chars_with_info *)
  val from_chars: {start: int, stop: int, get: int -> char} -> R.real option
  val from_string: string -> R.real option
end =
```

## Example Usage

Example `.mlb` file:
```
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/basis/mlton.mlb    (* for MLton.Real64 *)
lib/github.com/shwestrick/sml-fast-real/sources.mlb
main.sml
```

Example `main.sml`:
```sml
structure R64 =
struct
  open MLton.Real64 (* need this for fromLargeWord *)
  open Real64
end

structure FR = FastReal(R64)

val r = valOf (FR.from_string "123.456E-1")
```
