object P21 {
  def insertAt[A](elem:A, n:Int, ls:List[A]):List[A] =
    if (n < 0 || n > ls.length)
      throw new IndexOutOfBoundsException
    else {
      val (pre, post) = ls.splitAt(n)
      pre:::elem::post
    }

  def insertAtTail[A](elem:A, n:Int, ls:List[A]):List[A] = {
    def insertHelper(n:Int, ls:List[A], ret:List[A]):List[A] = (n,ls) match {
      case (0, _)              => ret.reverse:::elem::ls
      case (_, Nil) if (n > 0) => throw new IndexOutOfBoundsException
      case (_, h::tail)        => insertHelper(n-1, tail, h::ret)
    }

    if (n < 0)
      throw new IndexOutOfBoundsException
    else {
      insertHelper(n, ls, Nil)
    }
  }
}
