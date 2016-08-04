
/**
 * (*) Find the last but one element of a list.
    Example:

    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
 */
object P2 {
  
  def penultimate[A](list :List[A]) : A = {
    list match {      
      case head :: tail => if (tail.size == 1) head else penultimate(tail)
      case Nil => throw new NoSuchElementException
    }
    
  }
}