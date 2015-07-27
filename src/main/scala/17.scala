object P17 {
  def split[A](n:Int, ls:List[A]):(List[A], List[A]) = {
    def helper(i:Int, front:List[A], end:List[A]):(List[A], List[A]) = (i, end) match {
      case (_, Nil)     => (front.reverse, Nil)
      case (0, _)       => (front.reverse, end)
      case (_, h::tail) => helper(i-1, h::front, tail)
    }

    helper(n, Nil, ls)
  }
}
