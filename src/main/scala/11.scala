object P11 {
  import P10.encode

  private def encodeHelper(l:List[Any], i:Int, curr:Any, out:List[Any]):List[Any] = l match {
    case Nil =>
      if (i == 1)
        curr::out
      else
        (i, curr)::out
    case h::tail =>
      if (h == curr)
        encodeHelper(tail, i+1, h, out)
      else
        if (i == 1)
          encodeHelper(tail, 1, h, curr::out)
        else
          encodeHelper(tail, 1, h, (i, curr)::out)
  }

  def encodeModified(l:List[Any]) = l match {
    case Nil     => Nil
    case h::tail => encodeHelper(tail, 1, h, Nil).reverse
  }

  // this type safety is awesome
  def encodeModified2[A](l:List[A]): List[Either[A, (Int, A)]] =
    encode(l) map {
      x =>
      if (x._1 == 1)
        Left(x._2) // left refers to the left arg in the Either statement
      else
        Right(x)
    } // right refers to the right arg in the Either statement
}
