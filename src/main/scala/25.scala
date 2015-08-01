object P25 {
  import P23.randomSelect
  import scala.reflect._

  def randomPermute[A](ls:List[A]):List[A] = randomSelect(ls.length, ls)

  def randomPermute2[A:ClassTag](ls:List[A]): List[A] = {
    val rand = new util.Random
    // this does not compile, I have no Idea how to fix this
    // except that it has something to do with ClassTags
    // for some reason, scala cannot infer that this is of type Array[A]?
    val arr:Array[A] = ls.toArray(classTag[A])
    for (i <- 0 to arr.length - 1 by 1) {
      val il = rand.nextInt(i+1)
      val t = arr(i)
      arr.update(i, arr(il))
      arr.update(il, t)
    }
    arr.toList
  }
}
