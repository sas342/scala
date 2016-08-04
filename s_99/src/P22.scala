
/**
 * (*) Create a list containing all integers within a given range.
    Example:

    scala> range(4, 9)
    res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object P22 {
  def range(start:Int, end:Int) :List[Int] = {
    for (i <- (start to end).toList) yield {i}
  }
}