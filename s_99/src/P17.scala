
/**
 * (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.

    Example:

    scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 */
object P17 {
  def split[A](n:Int, list: List[A]):(List[A],List[A]) = {
    def sub(c:Int, l:List[A], n:List[A]) : (List[A],List[A]) = (c,l) match {
      case (_, Nil) => (n.reverse, Nil)
      case (0,_) => (n.reverse,l)      
      case (x,y) => sub(x-1, y.tail, y.head :: n)
     
    }
    
    sub(n, list, Nil)
   
  }
}