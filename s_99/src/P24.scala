
/**
 * (*) Lotto: Draw N different random numbers from the set 1..M.
    Example:

    scala> lotto(6, 49)
    res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */
object P24 {
  def lotto(count:Int, max:Int):List[Int] = {
    val random :java.util.Random = new java.util.Random();
    
    for (i <- (1 to count).toList) yield {random.nextInt(max)}
  }
}