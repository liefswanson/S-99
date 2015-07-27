object P16 {
  def drop[A](n:Int, ls:List[A]):List[A] = {
    def helper(i:Int, ls:List[A]):List[A] = (i, ls) match {
      case (0, h::tail) =>    helper(n-1, tail)
      case (_, h::tail) => h::helper(i-1, tail)
      case (_, Nil)     => Nil
    }
 
    helper(n-1, ls)
  }

  // zipwithindex is the key here the index is ._2
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    // ._2 + 1 is because of zero indexing
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }
}
