object P19 {
  def rotate[A](n:Int, ls:List[A]):List[A] = ls match {
    case Nil => Nil
    case _   =>
      val nBounded = if (n < 0) ls.length + n else n
      val (a, b) = ls.splitAt(nBounded % ls.length)
      b:::a
  }
}
