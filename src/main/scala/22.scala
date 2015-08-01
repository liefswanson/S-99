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

  // with step!
  def range(a:Int, b:Int, step:Int):List[Int] = {
    val iterate =
      if (a<b) (x:Int) => x+step
      else     (x:Int) => x-step

    def rangeHelper(a:Int, b:Int, ls:List[Int]):List[Int] = b-a match {
      case n if n.abs < step => (a::ls).reverse
      case _                 => rangeHelper(iterate(a), b, a::ls)
    }
    rangeHelper(a, b, Nil)
  }

  def range2(a:Int, b:Int):List[Int] = a to b toList
  def range2(a:Int, b:Int, step:Int):List[Int] = a to b by step toList
}
