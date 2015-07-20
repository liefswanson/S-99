object P01 {

  def lastRecursive[A](a:List[A]):A = a match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }

  def lastProcedural[A](a:List[A]):A = {
    a.last
  }
}

