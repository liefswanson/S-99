object P14 {
  def duplicate[A](l:List[A]):List[A] = l match {
    case Nil     => Nil
    case h::tail => h::h::duplicate(tail) 
  }
}
