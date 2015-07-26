object P12 {
  def decode[A](l:List[(Int,A)]):List[A] = l flatMap { x => List.fill(x._1)(x._2)}
}
