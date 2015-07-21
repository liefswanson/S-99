object P02 {
  // penultimate
  def penultimateRecursive[A](a:List[A]):A = a match {
    case _ :: m :: Nil => m
    case _ :: tail     => penultimateRecursive(tail)
    case _             => throw new NoSuchElementException
  }
  
  def penultimateProcedural[A](a:List[A]):A = {
    a.init.last
  }

  def penultimateProcedural2[A](a:List[A]):A = {
    a.reverse.tail.head
  }
}
