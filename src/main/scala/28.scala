object P28 {
  import P10.encode

  def lsort[A](ls:List[List[A]]): List[List[A]] =
    ls sortWith {_.length < _.length}
}
