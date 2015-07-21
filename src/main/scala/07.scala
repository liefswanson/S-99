object P07 {
  // inefficient due to concatentation at the end... I think
  // each time a list is found, the append is O(sublist_length)
  // worst case this becomes O(n*nest_depth)
  def flattenInefficient(l:List[Any]):List[Any] = l match {
    case Nil             => Nil
    case (h: List[_])::t =>
      flattenInefficient(h) ::: flattenInefficient(t)
    case h::t            => h :: flattenInefficient(t)
  }

  // may be a faster way
  def flattenMapStyle(l:List[Any]):List[Any] = l flatMap {
    case out: List[_] => flattenMapStyle(out)
    case base         => List(base)
  }

  // List[Any].flatten deprecated?
  // not sure where to find out about this, but it did not work in the repl
}
