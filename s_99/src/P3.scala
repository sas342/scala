

object P3 {
  def nth[A](i:Int, list:List[A]) :A = {
    (i ,list) match {
      case (0, head :: _) => list.head
      case b if ((b._1 > 0) && (b._2.tail != Nil)) => nth(i-1, list.tail)
      case _ => throw new NoSuchElementException
    }
    
  }
}