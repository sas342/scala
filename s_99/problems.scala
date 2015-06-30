
@annotation.tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B) : B = {
	l match {
		case Nil => z
		case head :: tail => foldLeft(tail, f(z, head))(f)
	}
}

def foldRight[A,B](l: List[A], z:B)(f: (A,B) => B): B =
	l match {
		case Nil => z
		case head :: tail => f(head, foldRight(tail,z)(f))
	}

def append[A](a1: List[A], a2: List[A]): List[A] = {
	val f = (a:A, b:List[A]) => a :: b
	foldRight(a1, a2)(f)
}

def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
	val z = (a:A, b:List[B]) => append(f(a), b)
	foldRight(l, Nil:List[B])(z)
}

//problem 5
def reverse[A](list: List[A]): List[A] = {
	val f = (b:List[A], a:A) => a :: b
	foldLeft(list, List[A]())(f)
}



//problem 6: same forward as backwards
def isPalindrome[A](list: List[A]): Boolean = {
	list == reverse(list)
}




//problem 7: flatten list of lists to single list
//A is a list of B
def flatten(l: List[Any]): List[Any] = l flatMap {
	
	case li :List[Any] => flatten(li)
	case e => List[Any](e)
	
}


//problem 8: Eliminate consecutive duplicates of list elements.
def compress[A](l: List[A]): List[A] = {
	val f= (a:A, b:List[A]) => if (b.length == 0 || a != b.head) a :: b else b
	foldRight(l, Nil:List[A])(f)
}


//problem 9: pack consecutive duplicates into sublists
def pack[A](list: List[A]):List[List[A]] = 
	foldRight(list, Nil:List[List[A]]){ (a:A,b:List[List[A]]) => 
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

//problem 10: encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
def encode[A](list: List[A]):List[(Int,A)] = {
	val packed_list = pack_improved(list)
	foldRight(packed_list, Nil:List[(Int,A)]){(a:List[A],b:List[(Int,A)]) => (a.length, a.head) :: b}
}

//problem 11: modified run-length encoding
def encodedModified[A](list: List[A]):List[Any] = {
	val packed_list = pack_improved(list)
	foldRight(packed_list, Nil:List[Any]){(a:List[A],b:List[Any]) => a match {
		case h :: Nil => h :: b
		case h :: tail => (a.length, h) :: b
		case Nil => Nil
	}}

}

//problem 12: decode run-length encoded list, try to use flatmap
def decode[A](list :List[(Int,A)]) :List[A] = 	
 	flatMap(list)((a:(Int,A)) => {for (i <- 1 to a._1) yield a._2}.toList)


//problem 13: run-length encoding of list (direct)
def encodeDirect[A](list: List[A]):List[(Int,A)] =
	foldRight(list, Nil:List[(Int,A)]){ (a:A,b:List[(Int,A)]) => 
		if (b.length == 0) (1,a) :: b 
		else if (b.head._2 == a) {
			val t = (b.head._1+1,a)
			t :: (b.drop(1))
		}
		 else (1,a) :: b
	}


//problem 14: duplicate elements in list
def duplicate[A](list: List[A]): List[A] = 
	foldRight(list, Nil:List[A]){(a:A,b:List[A]) => a :: a :: b}

//problem 15: duplicate elements n times, this time using fill instead of for loop yield
def duplicateN[A](n:Int, list:List[A]) : List[A] =
	flatMap(list){(a:A) => List.fill(n)(a)}	

//problem 16: drop every nth element from list
def drop[A](n:Int, list: List[A]):List[A] = {
	if (list.isEmpty) Nil
	else {
		val s:(List[A],List[A]) = list splitAt(n-1) 
		if (s._2 != Nil) s._1 ::: drop(n,s._2.tail) else s._1 
	}
}

//problem 17: split list into two parts
def split[A](n:Int, list: List[A]):(List[A],List[A]) = 
	def sub(c:Int, l:List[A], n:List[A]) = l match {
		
	}
	

println("reverse of (1,2,3,4) is "+reverse(List(1,2,3,4)))
println("isPalindrome (1,2,3) is "+isPalindrome(List(1,2,3)))
println("isPalindrome (1,2,1) is "+isPalindrome(List(1,2,1)))
println("flatMap(List(List(1, 1), 2, List(3, List(5, 8)))) is " + flatten(List(List(1,1),2,List(3,List(5,8)))))
println("removing dups of compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) "+compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println("pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = "+pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
println("pack_improved(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = "+pack_improved(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
println("encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = "+encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println("encodedModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = "+encodedModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println("decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) = "+decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
println("encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = "+encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println("duplicate(List('a, 'b, 'c, 'c, 'd)) = "+duplicate(List('a, 'b, 'c, 'c, 'd)))
println("duplicateN(3, List('a, 'b, 'c, 'c, 'd)) = "+duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
println("drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = "+drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
