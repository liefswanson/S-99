object P03 {
  def kthHelper[A](k:Int, a:List[A] ):A = (k, a) match {
    case (0, h::_)    => h
    case (k, _::tail) => kthHelper(k-1, tail)
    case (k, _)       => throw new NoSuchElementException
  }
}
