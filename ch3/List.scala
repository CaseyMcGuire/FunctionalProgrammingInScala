
//Functional linked list

//List datatype parametrized by type A
//traits in Scala are like Java interfaces that also may have concrete method implementations
//sealed means all implementations of this trait must be in this file
//The +A means that A is a covariant of List. That is, if X is a subtype of Y, then List[X] is a subtype of
//List[Y]
sealed trait List[+A] 
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

//Data types in Scala often have companion objects with the same name that contain convenience functions for working
//on the datatype. Companion objects are merely a convention and aren't strictly required.
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  //For data types in Scala, its a common idiom to have an apply method that can be used to create an instance of
  //the data type with the passed elements.
  def apply[A](as: A*): List[A] = //this is Scala's variadic function notation
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //3.2
  //Implement the function tail for removing the first element of a List.
  //We'll visit what to do if the head of the list is Nil in the next chapter
  def tail[A](lst: List[A]):List[A] = lst match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  //3.3
  //Implement the function setHead for replacing the first element of a List with a different value.
  def setHead[A](newHead: A, lst: List[A]): List[A] = lst match {
    case Nil => Cons(newHead, Nil)
    case Cons(_,xs) => Cons(newHead, xs)
  }

  //3.4
  //Generalize tail to the function drop, which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  //3.5
  //Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def dropWhile(l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs) else xs
    }
    dropWhile(l)
  }

  //Note that with our implementation here, we have to state the type of the parameter when calling 
  //dropwhile
  val xs: List[Int] = List(1,2,3,4,5)
  val ex1 = dropWhile(xs, (x: Int) => x < 4)

  //alternatively, you can write a curried version of dropWhile like so
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = 
    as match {
      case Cons(h,t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  //Essentially, what this is saying is that dropWhile is function that takes a List and returns another
  //function. This other function in turn takes a function and returns a list
  //Since this version is curried, we don't have annotate the lambda function
  val xs2 = List(1,2,3,4,5)
  val ex2 = dropWhile2(xs)( x => x < 4)

  //3.6
  //Implement a function init that returns a list consisting of all but the last element of a list
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new Exception("Empty list")
      case Cons(x,Nil) => Nil
      case Cons(x,xs) => Cons(x, init(xs))
    }
  }

  //If we look at our sum and product functions, we can see how similar they are. They essentially
  //are recursing down the list calling a different function on the head and the tail at each iteration
  //Whenever you see such duplication, you generalize it by pulling subexpressions out into function
  //arguments.
  //If the subexpression refers to any local variables, turn the subexpressions into a function that 
  //accepts these variables as arguments.

  //Note this function is not tail-recursive
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _)
  //Note: (_ * _) is more concise notation for (x,y) => x * y

  //Exercise 3.8
  //println(foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))
  //You just get the same List back

  //Exercise 3.9
  //Compute the length of a list using foldRight.
  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((x,y) => 1 + y)

  //Exercise 3,10
  //Write a general list-recursion function, foldLeft, that is tail-recursive
  def foldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    @scala.annotation.tailrec
    def foldLeft(acc: B, lst: List[A]): B = {
      lst match {
        case Nil => acc
        case Cons(x,xs) => foldLeft(f(x,acc), xs)
      }
    }
    foldLeft(z,as)
  }

  //Exercise 3.11
  //Rewrite sum, product, and length to use foldLeft
  def sum3(ns: List[Int]): Int = 
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = 
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = 
    foldLeft(as, 0)((_,y) => 1 + y)

  //3.12
  //Write a function that returns the reverse of a list.
  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((x,acc) => Cons(x,acc))
  }


  //3.14
  //Write append in terms of foldLeft or foldRight
  def append[A](ns: List[A], ns2: List[A]): List[A] = {
    foldRight(ns, ns2)(Cons(_,_))
  }

  def append2[A](ns: List[A], ns2: List[A]): List[A] =
    foldLeft(reverse(ns), reverse(ns2))(Cons(_,_))

  //Exercise 3.15
  //Write a function that concantenates a list of lists into a single list. Its runtime should
  //be linear in the total length of all lists.
  def concat[A](lsts: List[List[A]]): List[A] = 
    foldRight(lsts, Nil: List[A])((x,acc) => append(x,acc))

  def concat2[A](lsts: List[List[A]]): List[A] =
    foldLeft(reverse(lsts), Nil: List[A])((x, acc) => append(x,acc))


  //Exercise 3.16
  //Write a function that transforms a list of integers by adding 1 to each element
  def addOne(lst: List[Int]): List[Int] = 
    foldRight(lst, Nil: List[Int])((x,acc) => Cons(x+1,acc))

  //Exercise 3.17
  //Write a function that turns each value in a List[Double] into a String. 
  def listToString(lst: List[Double]): List[String] = 
    foldRight(lst, Nil: List[String])((x,acc) => Cons(x.toString, acc))


  //Exercise 3.18
  //Write a function map that generalizes modifying each element in a list while maintaining the
  //structure of the list
  def map[A,B](as: List[A])(f: A => B): List[B] = 
    foldRight(as, Nil: List[B])((x,acc) => Cons(f(x),acc))

  //Exercise 3.19
  //Write a function filter that removes elements from a list unless they satisfy a given predicate
  def filter[A,B](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A])((x,acc) => if(f(x)) Cons(x,acc) else acc)

  //Exercise 3.20
  //Write a function flatMap that works like map except that the function given will return a list
  //instead of a single result, and that list should be inserted into the final resulting list.
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil: List[B])((x,acc) => append(f(x), acc))

  //Exercise 3.21
  //Write filter in terms of flatMap
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(i => if (f(i)) List(i) else Nil)

  //Exercise 3.22
  //Write a function that accepts two lists and constructs a new list by adding corresponding elements
  def merge(lst1: List[Int], lst2: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def merge(lst1: List[Int], lst2: List[Int], acc: List[Int]): List[Int] = {
      (lst1, lst2) match {
        case (Cons(x,xs), Cons(y,ys)) => merge(xs,ys,Cons(x+y, acc))
        case _ => reverse(acc)
      }
    }
    merge(lst1,lst2,Nil: List[Int])
  }

  //Exercise 3.23
  //Generalize the function you just wrote so that it's not specific to integers or addition. 
  def zipWith[A,B,C](lst1: List[A], lst2: List[B])(f: (A,B) => C): List[C] = {
    @scala.annotation.tailrec
    def zipWith(lst1: List[A], lst2: List[B], acc: List[C]): List[C] = {
      (lst1, lst2) match {
        case (Cons(x,xs), Cons(y,ys)) => zipWith(xs,ys,Cons(f(x,y), acc))
        case _ => reverse(acc)
      }
    }
    zipWith(lst1, lst2, Nil: List[C])
  }

  //Exercise 3.24
}

