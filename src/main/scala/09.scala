object P09 {
  private def packHelper[A](l:List[A], curr:List[A], out:List[List[A]]):List[List[A]] = (l, curr) match {
    case (Nil, _)     => curr::out
    case (h::tail, _) =>
      if (h == curr.head)
        packHelper[A](tail, h::curr, out)
      else
        packHelper[A](tail, List(h), curr::out)
  }

  def pack[A](l:List[A]):List[List[A]] = l match {
    case Nil     => Nil
    case h::tail => packHelper(tail, List(h), Nil).reverse
  }

  def pack2[A](l:List[A]):List[List[A]] = l match {
    case Nil => List(List())
    case _   =>
      val (packed, next) = l span {_ == l.head}
      if (next == Nil)
        List(packed)
      else
        packed::pack2(next)
  }
}
