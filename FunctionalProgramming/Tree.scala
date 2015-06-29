sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	/** non generalized */
	/**	
	def size[A](t: Tree[A]):Int = {
		t match {
			case s:Leaf[A] => 1
			case s:Branch[A] => 1 + size(s.left) + size(s.right)
		}
	}

	def maximum(t: Tree[Int]):Int = {
		t match {
			case s:Leaf[Int] => s.value
			case s:Branch[Int] => maximum(s.left) max maximum(s.right)
		}
	}

	def depth[A](t: Tree[A]):Int = {
		t match {
			case s:Leaf[A] => 0
			case s:Branch[A] => (depth(s.left) max depth(s.right)) + 1
		}
	}

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
		t match {
			case s:Leaf[A] => Leaf[B](f(s.value))
			case s:Branch[A] => Branch[B](map(s.left)(f),map(s.right)(f))
		}
	}
	*/

	/** Generalized */
	def size[A](t: Tree[A]):Int = {
		val f = (l:Int,r:Int) => 1 + l + r
		val z = (b:A) => 1		
		fold(t)(z)(f)
		
	}

	def maximum(t: Tree[Int]):Int = {
		val f = (l:Int,r:Int) => l max r
		val z = (b:Int) => b
		fold(t)(z)(f)
	}

	def depth[A](t: Tree[A]):Int = {
		val f = (l:Int,r:Int) => 1 + (l max r)
		val z = (b:A) => 0
		fold(t)(z)(f)
	}
	
	/*
Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:
type mismatch;
found : fpinscala.datastructures.Branch[B]
required: fpinscala.datastructures.Leaf[B]
fold(t)(a => Leaf(f(a)))(Branch(_,_))
^
This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
common to define helper functions that simply call the corresponding data constructors but give the less specific
result type:
def leaf[A](a: A): Tree[A] = Leaf(a)
def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
*/
	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
		val x = (l:Tree[B], r:Tree[B]) => Branch[B](l,r)
		val z = (a:A) => Leaf[B](f(a)): Tree[B]
		fold(t)(z)(x)
	}

	def fold[A,B](t: Tree[A])(z: A => B)(f: (B,B) => B): B = {
		t match {
			case s:Leaf[A] => z(s.value)
			case s:Branch[A] => f(fold(s.left)(z)(f), fold(s.right)(z)(f))
		}
	}
	

}

import Tree._
println(size(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))))
println(maximum(Branch(Branch(Leaf(1),Branch(Leaf(4),Leaf(3))),Leaf(5))))
println(maximum(Branch(Branch(Leaf(1),Branch(Leaf(4),Leaf(6))),Leaf(5))))
println(depth(Branch(Branch(Leaf(1),Branch(Leaf(4),Leaf(6))),Leaf(5))))
println(map(Branch(Branch(Leaf(1),Branch(Leaf(4),Leaf(6))),Leaf(5)))(_+1))
