structure FR = FastReal(Real64)

val s = List.hd (CommandLine.arguments ())
val _ = FR.from_chars
  {start = 0, stop = String.size s, get = fn i => String.sub (s, i)}
