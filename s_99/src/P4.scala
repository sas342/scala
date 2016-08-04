
/**
 * (*) Find the number of elements of a list.
    Example:

    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
 */
object P4 {
  def length[A](list: List[A]) = {
    val f = (b:Int, a:A) => {b+1}
    Common.foldLeft(list, 0)(f);
  }
}