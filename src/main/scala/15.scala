object P15 {
  def duplicateN[A](n:Int, l:List[A]):List[A] = l flatMap {x => List.fill(n)(x)}
}
