structure R64 =
struct (* need this for fromLargeWord *) open MLton.Real64 open Real64 end

structure FR = FastReal(R64)

val s = List.hd (CommandLine.arguments ())
val x = FR.from_chars
  {start = 0, stop = String.size s, get = fn i => String.sub (s, i)}

val () =
  case x of
    NONE => print ("NONE\n")
  | SOME {result, num_chomped} => print (Real.fmt StringCvt.EXACT result ^ "\n")
