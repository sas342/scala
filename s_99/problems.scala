
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
def compress(l: List[Any]): List[Any] = {
	val f= (a:Any, b:List[Any]) => if (b.length == 0 || a != b.head) a :: b else b
	foldRight(l, Nil:List[Any])(f)
}


println("reverse of (1,2,3,4) is "+reverse(List(1,2,3,4)))
println("isPalindrome (1,2,3) is "+isPalindrome(List(1,2,3)))
println("isPalindrome (1,2,1) is "+isPalindrome(List(1,2,1)))
println("flatMap(List(List(1, 1), 2, List(3, List(5, 8)))) is " + flatten(List(List(1,1),2,List(3,List(5,8)))))
println("removing dups of compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) "+compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
