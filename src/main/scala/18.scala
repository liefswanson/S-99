object P18 {
  def sliceCheating[A](a:Int, b:Int, ls:List[A]):List[A] = ls drop a take b-(a max 0)

  def slice[A](a:Int, b:Int, ls:List[A]):List[A] = {
    def take(n:Int, ls:List[A]):List[A] = (n, ls) match {
      case (_, Nil) => Nil
      case (0, _) => Nil
      case (i, h::tail) => h::take(i-1, tail)
    }
    def drop(n:Int, ls:List[A]):List[A] = (n, ls) match {
      case (_, Nil) => Nil
      case (0, _) => ls
      case (i, h::tail) => drop(i-1, tail)
    }
    take(b-a, drop(a, ls))
  }
}
