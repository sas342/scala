
/**
 * (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.

    Example:

    scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
 */
object P26 {
  
  def combinations[A](number:Int, list:List[A]) :List[List[A]] = {
    
    def combo_inner(totalList:List[List[A]], current:A) :List[List[A]]= {
      //add current to all in total list
      var appended:List[List[A]] = totalList
      
      if (totalList.isEmpty)
      {
        val t = current :: Nil
        return t :: Nil 
      }
      
      //add itself
      val t = current :: Nil
      appended = t :: appended
      
      for (a <- totalList)
      {
        val t = current :: a 
        appended = t :: appended
      }
      return appended
    }
    
    var total :List[List[A]] = Nil
    for (a <- list)
    {
      total = combo_inner(total, a)
    }
    return total.filter { x => x.size == 3 };
  }
  
}