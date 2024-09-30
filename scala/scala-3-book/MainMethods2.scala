@main
def repeat(word: String, times: Int) =
  for
    i <- 1 to times
  do println(word)
