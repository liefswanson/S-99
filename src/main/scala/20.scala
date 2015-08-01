object P20 {
  def removeAt[A](n:Int, ls:List[A]):(List[A],A) =
    if (n < 0) throw new NoSuchElementException
  else ls.splitAt(n) match {
    case (_, Nil)       => throw new NoSuchElementException
    case (pre, h::post) => (pre:::post, h)
  }

  def removeAtTail[A](n:Int, ls:List[A]):(List[A], A) = {

    def removeHelper(n:Int, ls:List[A], ret:List[A]):(List[A], A) = (n, ls) match {
      case (_, Nil)     => throw new NoSuchElementException
      case (0, h::tail) => (ret.reverse:::tail, h)
      case (_, h::tail) => removeHelper(n-1, tail, h::ret)
    }

    if (n < 0)
      throw new NoSuchElementException
    else
      removeHelper(n, ls, Nil)
  }
}
