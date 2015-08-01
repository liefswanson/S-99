object P27 {
  import P26.combinations

  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls.filterNot(a.toSet)
      b <- combinations(3, noA)
    } yield List(a, b, noA.filterNot(b.toSet))

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls.filterNot(c.toSet)) map {c :: _}
    }
  }
}
