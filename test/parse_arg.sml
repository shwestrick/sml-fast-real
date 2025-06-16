structure FR = FastReal (struct open Real64 open MLton.Real64 end)

val {result, num_chomped, fast_path} = valOf (FR.from_string_with_info
  (List.hd (CommandLine.arguments ())))
val _ = print ("result      " ^ Real.toString result ^ "\n")
val _ = print ("num_chomped " ^ Int.toString num_chomped ^ "\n")
val _ = print ("fast_path   " ^ (if fast_path then "true" else "false") ^ "\n")
