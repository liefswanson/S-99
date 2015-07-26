object P10 {
  import P09.pack

  private def encodeHelper[A](l:List[A], i:Int, curr:A, out:List[(Int, A)]):List[(Int, A)] = l match {
    case Nil => (i,curr)::out
    case h::tail =>
      if (h == curr)
        encodeHelper(tail, i+1, h, out)
      else
        encodeHelper(tail, 1, h, (i, curr)::out)
  }

  def encode[A](l:List[A]):List[(Int, A)] = l match {
    case Nil     => Nil
    case h::tail => encodeHelper(tail, 1, h, Nil).reverse
  }

  def encode2[A](l:List[A]): List[(Int, A)] =
    pack(l) map {x => (x.length, x.head)}
}
