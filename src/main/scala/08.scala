object P08 {
  private def compressHelper[A](l:List[A], curr:A, out:List[A]):List[A] = l match {
    case Nil => out
    case h::tail =>
      if (h == curr)
        compressHelper(tail, h, out)
      else
        compressHelper(tail, h, h::out)
  }

  // this is a mistake, I meant to define the return type,
  // but cannot without altering the function now
  def compress[A](l:List[A]) = compressHelper(l, Nil, Nil).reverse

  def compress2[A](l:List[A]):List[A] = l match {
    case Nil     => Nil
    case h::tail => h::compress2(tail.dropWhile(_ == h)) // don't understand how we can compare to _
  }
}
