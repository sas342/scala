

object Common {
  
  
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

}