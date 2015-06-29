
@annotation.tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B) : B = {
	l match {
		case Nil => z
		case head :: tail => foldLeft(tail, f(z, head))(f)
	}
}

//problem 5
def reverse[A](list: List[A]): List[A] = {
	val f = (b:List[A], a:A) => a :: b
	foldLeft(list, List[A]())(f)
}


//problem 6: 
def isPalindrome[Int](list: List[A]): Bool = {

}

println("reverse of (1,2,3,4) is "+reverse(List(1,2,3,4)))