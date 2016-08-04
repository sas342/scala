

/**
 * (*) Reverse a list.
    Example:

    scala> reverse(List(1, 1, 2, 3, 5, 8))
    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */
object P5 {

  def reverse[A](list: List[A]): List[A] = {
	  val f = (b:List[A], a:A) => a :: b
	  Common.foldLeft(list, List[A]())(f)
  }
}  