
/**
 * (*) Duplicate the elements of a list.
    Example:

    scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object P14 {
  def duplicate[A](list: List[A]): List[A] = 
	  Common.foldRight(list, Nil:List[A]){(a:A,b:List[A]) => a :: a :: b}

}