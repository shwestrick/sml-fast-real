structure FR = FastReal(Real64)

val s = List.hd (CommandLine.arguments ())
val x = FR.from_chars
  {start = 0, stop = String.size s, get = fn i => String.sub (s, i)}

val () =
  case x of
    NONE => print ("NONE\n")
  | SOME {result, num_chomped} => print (Real.fmt StringCvt.EXACT result ^ "\n")
