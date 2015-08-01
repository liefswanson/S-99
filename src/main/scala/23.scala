object P23 {
  import P20.removeAt
  // this function is really slow
  // running time is O(ls.length*n)
  // worst case is O(min(ls.length, n)**2) which sucks...
  // best case is O(n) but that isn't much consolation
  def randomSelect[A](n:Int, ls:List[A]):List[A] = {
    val rand = new util.Random
    def rsHelper(n:Int, len:Int, ls:List[A], ret:List[A]):List[A] = (n, ls) match {
      case (0, _)   => ret
      case (_, Nil) => ret
      case (_, _)   =>
        val (other, elem) = removeAt(rand.nextInt(len), ls)
        rsHelper(n-1, len-1, other, elem::ret)
    }
    rsHelper(n, ls.length, ls, Nil)
  }
}
