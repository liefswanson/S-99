object P13 {
  private def encodeHelper[A](l:List[A], i:Int, curr:A):List[(Int, A)] = l match {
    case Nil => Nil
    case h::tail =>
      if (h == curr)
        encodeHelper(tail, i+1, h)
      else
        (i,curr)::encodeHelper(tail, 1, h)
  }

  def encodeDirect[A](l:List[A]):List[(Int, A)] = l match {
    case Nil     => Nil
    case h::tail => encodeHelper(tail, 1, h)
  }
}
