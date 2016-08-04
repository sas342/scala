
/**
 * (**) Drop every Nth element from a list.
    Example:

    scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */
object P16 {
  def drop[A](n:Int, list: List[A]):List[A] = {
  	if (list.isEmpty) Nil
  	else {
  		val s:(List[A],List[A]) = list splitAt(n-1) 
  		if (s._2 != Nil) s._1 ::: drop(n,s._2.tail) else s._1 
  	}
  }
}