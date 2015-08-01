object P24 {
  import P23.randomSelect
  import scala.language.postfixOps

  def lotto(selections:Int, size:Int):List[Int] =
    if (selections > 0 && size >= selections)
      randomSelect(selections, 1 to size toList)
    else
      throw new IllegalArgumentException("Size of lottery must be greater than number of selection, and there must be at least one selection.")
}
