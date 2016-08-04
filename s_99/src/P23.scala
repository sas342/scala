
/**
 * (**) Extract a given number of randomly selected elements from a list.
    Example:

    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    res0: List[Symbol] = List('e, 'd, 'a)

    Hint: Use the solution to problem P20
 */
object P23 {
  
  
  def randomSelect[A](qty:Int, list:List[A]) :List[A] = {
    val random :java.util.Random = new java.util.Random()
    
    def random_inner(counter:Int, list:List[A], temp:List[A]) :List[A] = (counter,list) match {      
      case (0,_) => temp
      case (x,y) if (y.size == 0) => temp
      case (x,y) => {
        var tupl = P20.removeAt(random.nextInt(list.size), list)
        random_inner(x-1, tupl._1, tupl._2 :: temp)
      }
    }
  
    random_inner(qty, list, Nil)
  }
}