object P05 {
  def reverseHelper[A](l:List[A], r:List[A]):List[A] = l match {
    case Nil => r
    case l::t => reverseHelper(t, l::r)
  }

  def reverse[A](l:List[A]):List[A] = reverseHelper(l, List())
}
