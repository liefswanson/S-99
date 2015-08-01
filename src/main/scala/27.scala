object P27 {
  import P26.combinations

  def diff[A](ls: List[A], ls2: List[A]): List[A] = ls.filterNot(ls2.toSet) 

  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = diff(ls, a)
      b <- combinations(3, noA)
    } yield List(a, b, diff(noA, b))

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls.filterNot(c.toSet)) map {c :: _}
    }
  }
}
