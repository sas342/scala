//isSorted  Array[A] => Boolean

def max(x:Int, y:Int) : Boolean = {
	x > y
}

def isSorted[A](arry:Array[A], gt: (A,A) => Boolean):Boolean = {
	def go(i:Int):Boolean = {
		if (i == arry.length) true
		else go(i+1) & gt(arry(i), arry(i-1))
	}	
	
	go(1)
	//var b = true
	//for (i <- 1 to arry.length -1) {
	//	if (!b) false
	//	else { println(s"comparing ${arry(i)} and ${arry(i-1)}"); b = gt(arry(i),arry(i-1))}
	//}
	//b
}

println(isSorted(Array(1,2,3,4,5,6,7,8,9), max))
println(isSorted(Array(1,2,3,4,6,5,7,9), max))
println(isSorted(Array(1,2,3,4,5,6,9,8), max))

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
	def d(b:B):C = f(a,b)
	d
	
}

def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
	a => b => f(a,b) 
}

def uncurry[A,B,C](f:A => B => C): (A,B) => C =
	(a,b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C = {
	a => f(g(a))
}	
