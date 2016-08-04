
/**
 * (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.

    Example:

    scala> removeAt(1, List('a, 'b, 'c, 'd))
    res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */
object P20 {
  def removeAt[A](n:Int, list:List[A]) :(List[A],A) = {
    
    def remove_inner(counter:Int, temp:List[A], list:List[A]) :(List[A],A) = (counter, list) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, head :: tail) => (temp.reverse ::: tail, head)
      case (x, head :: tail) => remove_inner(x-1, head :: temp, tail) 
    }
    
    remove_inner(n, Nil, list)
  }
}