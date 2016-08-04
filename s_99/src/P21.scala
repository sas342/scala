
/**
 * (*) Insert an element at a given position into a list.
    Example:

    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */
object P21 {
  
  def insertAt[A](entry:A, location:Int, list:List[A]):List[A] =
  {
    def insert_inner(counter:Int, temp:List[A], list:List[A]):List[A] = (counter, list) match {
      case (_,Nil) => Nil
      case (0,head :: tail) => temp ::: (entry :: tail)
      case (x,head :: tail) => insert_inner(x-1,head :: temp, tail)
    }
    
    insert_inner(location, Nil, list)
  }
}