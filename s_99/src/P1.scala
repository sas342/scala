
/**
 * P01 (*) Find the last element of a list.
    Example:

    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
 */
object P1 {
  
  def last[A] (a: List[A]) : A = {
    
    a match {
      case head :: tail => if (tail.size == 0)  head else last(tail)
      case Nil => throw new NoSuchElementException
    }
  }
}