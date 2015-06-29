sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	
	def sum(ints: List[Int]): Int = {
		foldLeft(ints, 0)(_+_)
	}
	
	def product(ds: List[Double]): Double = {
		foldLeft(ds, 1.0)(_ * _)		
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		val f = (a:A, b:List[A]) => Cons(a, b) 
		foldRight(a1, a2)(f)
	}
	/**
	* old 
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
		
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		a1 match {
			case Nil => a2
			case Cons(h,t) => Cons(h, append(t, a2))
		}
	}
	*/

	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def tail[A](ints: List[A]): List[A] = ints match {
		case Nil => Nil
		case Cons(x,xs) => xs
	}

			
	def drop[A](l: List[A], n:Int): List[A] = {
		if (n <= 0) l
		else l match {
			case Nil => Nil
			case Cons(_,xs) => drop(xs, n-1)
		}
		
		
	}

	def dropWhile[A](l: List[A])(f: A => Boolean) : List[A] = {
		
		l match {
			case Cons(x,xs) if f(x) => dropWhile(xs)(f)
			case _ => l
			
		}
	}

	

	def setHead[A](a:A, l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(h,xs) => Cons(a, xs)
	}
	
	def init[A] (l: List[A]): List[A] = l match{
		case Cons(h,xs) if xs == Nil => Nil
		case Cons(h,xs) => Cons(h,init(xs))
	}
	
	def foldRight[A,B](l: List[A], z:B)(f: (A,B) => B): B =
		l match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs,z)(f))
		}

	
	def length[A](l: List[A]): Int = {
		val f = (b:Int, a:A) => b + 1
		//foldRight(l, 0)(f)
		foldLeft(l, 0)(f)
	}

	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B) : B = {
		l match {
			case Nil => z
			case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
		}
	}

	def reverse[A](l: List[A]): List[A] = {
		val f = (b:List[A], a:A) => Cons(a, b) 
		foldLeft(l, List[A]())(f)
	}
	
	//list of lists into single list
	def concatenate[A](l: List[List[A]]): List[A] = {
		//val z = (b:A, a:List[A]) => Cons(b,a)
		//val f = (a:List[A], b:List[A]) => foldRight(a,b)(z)
		foldRight(l, List[A]())(append)
	}

	def transform(l:List[Int]): List[Int] = {
		val f = (a:Int, b:List[Int]) => Cons((a + 1),b)
		foldRight(l, Nil:List[Int])(f)
	}

	def dToString(l:List[Double]): List[String] = {
		val f = (a:Double, b:List[String]) => Cons(a.toString, b)
		foldRight(l, Nil:List[String])(f)
	}

	def map[A,B](l: List[A])(f: A => B): List[B] = {
		val z = (a:A, b:List[B]) => Cons(f(a),b)
		foldRight(l, Nil:List[B])(z)
	}

	def filter[A](l: List[A], p: A => Boolean): List[A] = {
		val f = (a:A, b:List[A]) => if (p(a)) Cons(a,b) else b
		foldRight(l, Nil:List[A])(f)
	}

	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
		val z = (a:A, b:List[B]) => append(f(a),b)
		foldRight(l, Nil:List[B])(z)
	}

	def addLists[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1,l2) match{
		
			case (Nil,_) => Nil
			case (_,Nil) => Nil
			case (Cons(ah,at),Cons(bh,bt)) => Cons(f(ah, bh), addLists(at,bt)(f))			
		
	}
}

import List._
val example2 = List(1,2,3)
println(example2)
val total = sum(example2)
println(total)
val t = tail(List(1,2,3,4))
println(t)
//println(t.size)
println("product of (1,2,3,4)")
println(product(List(1,2,3,4)))


val t2 = drop(List(1,2,3,4),2)
println(t2)
println(setHead(4,List(1,2,3,4)))
println(init(List(1,2,3,4)))
println(length(List(1,2,3)))

println("reversing (1,2,3)")
println(reverse(List(1,2,3)))
println("appending (3,4) to (1,2)")
println(append(List(1,2), List(3,4)))

println("concatenating (1,2),(3,4,5),(6,7,8,9)")
println(concatenate(List(List(1,2),List(3,4,5),List(6,7,8,9))))
println("trasnform (1,2,3,4) to (2,3,4,5)")
println(transform(List(1,2,3,4)))
println(dToString(List(1.0,2.0,3.5)))

println(filter(List(1,2,3,4,5,6,7,8,9),(i:Int) => i % 2 == 1))
println(flatMap(List(1,2,3))(i => List(i,i)))
println(addLists(List(1,2,3),List(4,5,6))(_ + _))
println(length(reverse(List(1,2,3))))
