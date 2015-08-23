import scala.annotation._
object Ch2 {

  //by putting the tailrec annotation, we are telling the Scala compiler to check to make sure this function is
  //tail recursive
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = 
      if(n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  //2.1
  //Write a recursive function to get the nth Fibonacci number
  def fibonacci(nth: Int):Int = {
    @tailrec
    def fibonacci(iter: Int, prevFib: Int, curFib: Int): Int =
      if(iter == nth) curFib
      else fibonacci(iter + 1, curFib, curFib + prevFib)

    if(nth == 0) 0
    else if(nth == 1) 1
    else fibonacci(1, 0, 1)
  }


  //This is a higher order function that takes another function as a parameter.
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  //Monomorphic functions operate on one type of data. Polymorphic functions operate on many types of data

  //Polymorphic function to find an element in an array
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = 
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }


  //2.2
  //Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = 
      if(i == as.length - 1) true
      else if (ordered(as(i),as(i+1))) loop(i + 1)
      else false

    loop(0)
  }

  //Let's take a look at a function that used for partial application. It takes a valule and a function of two 
  //arguments, and returns a function of one argument as its result. 
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = 
    (b: B) => f(a, b)

  //2.3
  //Let's look at another example, currying, which converts a function of f of two arguments into a function of one
  //argument that partially applies f.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    (a: A) => ((b: B) => f(a,b))


  //2.4
  //Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, A => 
  //(B => C) can be written as A => B => C.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
    (a: A, b: B) => f(a)(b)

  //2.5
  //Implement the higher-order function that composes two functions
  def compose[A,B,C](f: B => C, g: A => B): A => C = 
    (a: A) => f(g(a))
}
