object P26 {
  import scala.language.postfixOps

  // this is a very strange solution, not sure how else it could be done
  def flatMapSublists[A](ls: List[A])(callback: List[A] => List[List[A]]):List[List[A]] =
    ls match {
      case Nil         => Nil
      case (_ :: tail) => callback(ls) ::: flatMapSublists(tail)(callback)
  }

  def combinations[A](len:Int, ls:List[A]):List[List[A]] = {
    if (len == 0)
      List(Nil)
    else
      flatMapSublists(ls)({
        subls =>
        combinations(len-1, subls.tail) map {subls.head::_}
      })
  }


}
