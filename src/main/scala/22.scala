object P22 {
  def range(a:Int, b:Int):List[Int] = {
    val iterate =
      if(a > b) (x:Int) => x + 1
      else      (x:Int) => x - 1
      
    def rangeHelper(a:Int, b:Int, ls:List[Int]):List[Int] = b-a match {
      case 0 => b::ls
      case _ => rangeHelper(a, iterate(b), b::ls)
    }
    rangeHelper(a, b, Nil)
  }
}
