
/**
 *  (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:

    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    
 */
object P9 {
  //problem 9: pack consecutive duplicates into sublists
  def pack[A](list: List[A]):List[List[A]] = 
  	Common.foldRight(list, Nil:List[List[A]]){ (a:A,b:List[List[A]]) => 
  		if (b.length == 0) List[A](a) :: b 
  		else if (b.head.head == a) {
  			val t = a :: b.head; 
  			t :: (b.drop(1))
  		}
  		 else List[A](a) :: b
  	}
  
  //problem 9a: improved with using list span method
  def pack_improved[A](list: List[A]):List[List[A]] = {
  	if (list.isEmpty) Nil
  	else {
  		val packed = list span (_ == list.head)			
  		packed._1 :: pack_improved(packed._2)
  	}
  	
  }
}