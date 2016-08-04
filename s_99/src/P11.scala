
/**
 * (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */
object P11 {
  def encodedModified[A](list: List[A]):List[Any] = {
  	val packed_list = P9.pack_improved(list)
  	Common.foldRight(packed_list, Nil:List[Any]){(a:List[A],b:List[Any]) => a match {
  		case h :: Nil => h :: b
  		case h :: tail => (a.length, h) :: b
  		case Nil => Nil
  	}}
  
  }

}