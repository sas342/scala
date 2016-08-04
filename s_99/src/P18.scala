
/**
 * (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

    Example:

    scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */
object P18 {
  
  
  def slice[A](n1:Int, n2:Int, list:List[A]) :List[A] = {
    
    @annotation.tailrec
    def slice_inner(counter:Int, list:List[A], sliceList:List[A]) :List[A] = (counter,list) match {
      case (_,Nil) => Nil
      case (x,y) if (x > n1 && x <= n2) => slice_inner(x+1, y.tail, y.head :: sliceList)
      case (x,y) if (x > n2) => sliceList.reverse
      case (x,y) => slice_inner(x+1,y.tail,sliceList)
    }
    
    slice_inner(1,list,Nil)
  }
}