
/**
 * (**) Rotate a list N places to the left.
    Examples:

    scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */
object P19 {
  def rotate[A](n:Int, list:List[A]):List[A] = {
    
    def rotate_right(n:Int, list:List[A]):List[A] = (n,list) match {     
      case (_,Nil) => Nil
      case (c,head :: tail) if (c > 0) => rotate_right(c-1, tail ::: List(head))
      case (c,_) => list
    }
  
    if (n < 0)
    {
      //rotate_right(n* -1, list.reverse).reverse
      rotate_right(n+list.length, list)
    }
    else
      rotate_right(n, list)
  }    
    
}