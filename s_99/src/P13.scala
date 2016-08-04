
/**
 * (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.

    Example:

    scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object P13 {
  def encodeDirect[A](list: List[A]):List[(Int,A)] =
  	Common.foldRight(list, Nil:List[(Int,A)]){ (a:A,b:List[(Int,A)]) => 
  		if (b.length == 0) (1,a) :: b 
  		else if (b.head._2 == a) {
  			val t = (b.head._1+1,a)
  			t :: (b.drop(1))
  		}
  		 else (1,a) :: b
  	}

}