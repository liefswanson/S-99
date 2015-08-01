object P28 {
  import P10.encode

  def lSort[A](ls:List[List[A]]): List[List[A]] =
    ls sortWith {_.length < _.length}

  def lFreqSort[A](ls:List[List[A]]): List[List[A]] ={
    val freqs = Map(encode(ls map { _.length } sortWith { _ < _ }) map { _.swap }:_*)
    ls sortWith { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
  }
}
