import Stream._
//Look at his expression
//List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
//each function call creates a new list that is then passed on to the next function

//Wouldn't it be nice if we could somehow fuse sequences of transformations like this into
//a single pass and avoid creating temporary data structures?

//It turns out we can accomplish this kind of automatic loop fusion through the use of non-strictness
//(or, less formally, laziness). In this chapter, we'll explain what exactly this means, and we'll
//work through the implementation of a lazy list type that fuses sequences of transformations.

//Non-strictness is a property of a function. To say a function is non-strict just means that the 
//function may choose not to evaluate one or more of its arguments. In contrast, a strict function
//always evaluates its arguments.

//In Scala, we can write non-strict functions by accepting some of our arguments unevaluated. 
//Here's a non-strict function
object Example {
def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = 
  if (cond) onTrue() else onFalse()

//The arguments we'd like to pass unevaluated have a () => immediately before their type. A value 
//of type () => A is a function that accepts zero arguments and returns an A. In general, the
//unevaluated form of an expression is called a thunk, and we can force the thunk to evaluate
//the expression and get a result.

//Overall, this syntax makes it very clear what's happening-- we're passing a function of no 
//arguments in place of each non-strict parameter, and then explicitly calling this function
//to obtain a result in the body. In fact, the type () => A is a syntactic alias for the type
//Function0[A]

//This is such a common case that Scala provides some nicer syntax:
def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A = 
  if (cond) onTrue else onFalse
}

//A nonempty stream consists of a head and a tail, which are both non-strict. Due to technical
//limitations, these are thunks that must be explicitly forced, rather than by-name parameters.
sealed trait Stream[+A]{ 

  //here's a function to optionally extract the head of a Stream: 
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  //Exercise 5.1
  //Write a function to convert a Stream to a List, which will force its evaluation.

  //recursive version. O(n) time, O(n) space. Risks stack overflow
  def toList: List[A] = 
    this match {
      case Empty => Nil
      case Cons(h,t) => h()::t().toList
    }

  //tail recursive version. O(n) time, O(1) space but iterates through list twice
  def toList_2: List[A] = {
    @scala.annotation.tailrec
    def toList(str: Stream[A], acc: List[A]): List[A] = {
      str match {
        case Empty => acc.reverse
        case Cons(h,t) => toList(t(), h()::acc)
      }
    }
    toList(this,Nil)
  }

  //The book also has this solution that uses a mutable list to avoid extra iteration
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @scala.annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) => 
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  //exercise 5.2
  //Write the function take(n) for returning the first n elements of a Stream, and drop(n) for
  //skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = 
    (this, n) match {
      case (Empty, _) => empty
      case (_, 0) => empty
      case (Cons(h,t), remainder) => cons(h(), t().take(remainder - 1))
    }

  //Note: a function must be either final or private to be annotated as tail-recursive.
  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = 
    (this, n) match {
      case (Empty, _) => empty
      case (_, 0) => this
      case (Cons(h,t), remainder) => t().drop(remainder - 1)
    }

  //Exercise 5.3
  //Write the function takeWhile for returning all starting elements of a Stream that match the given
  //predicate.
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match {
      case Cons(h,t) => if(p(h())) cons(h(), t().takeWhile(p)) else cons(h(), Empty)
      case Empty => empty
    }

  //The arrow => in front of the argument type B means that the function f takes its second 
  //argument by name and may choose not to evaluate it
  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  //note that this implementation of exists, while illustrative, isn't stack safe
  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a,b) => p(a) || b)

  //Exercise 5.4
  //Implement forAll, which checks that all elements in the Stream match a given predicate. Your
  //implementation should terminate the traversal as soon as it encounters a nonmatching value
  @scala.annotation.tailrec
  final def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h,t) => if(!p(h())) false else t().forAll(p)
      case _ => true
    }

  //Exercise 5.5
  //Use foldRight to implement takeWhile
  def takeWhile_2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else Empty)

  //Exercise 5.6
  //Hard: Implement headOption using foldRight
  def headOption_2: Option[A] = 
    foldRight[Option[A]](None)((h, _) => Some(h))

  //Exercise 5.7
  //Implement map, filter, append, and flatMap using foldRight. The append method shoudl be
  //non-strict in its argument.
  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = 
    foldRight[Stream[A]](empty)((h, t) => if(f(h)) cons(h, t) else t)

  def append[B >: A](other: => Stream[B]): Stream[B] = 
    foldRight(other)((h,t) => cons(h,t))


  //Note that these implementations are incremental -- They don't fully generate their answers. It's
  //not until some other computation looks at the elements of the resulting Stream that the 
  //computation to generate that Stream actually takes place -- and then it will do just enough
  //work to generate the requested elements. 

  //Since we don't fully instantiate intermediate streams, it's exactly as if we had interleaved
  //the logic using a special-purpose loop. For this reason, people sometimes describe streams
  //as "first-class loops" whose logic can be combined using higher-order functions.

  //Since intermediate streams aren't instantiated, it's easy to reuse existing combinators in
  //novel ways without having to worry that we're doing more processing of the stream than 
  //necessary.

  //The incrememtal nature of stream transformation also has important consequences for memory usage.
  //Because intermediate streams aren't generated, a transformation of the stream requires only
  //enough working memory to store and transform the current element.

  def find(p: A => Boolean): Option[A] = 
    filter(p).headOption


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

    //Adding the lazy keyword to a val declaration will cause Scala to delay evaluation of the right-hand
    //side of that lazy val declaration until it's first referenced. It will also cache the result so
    //that subsequent references to it don't trigger repeated evaluation.
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  //Infinite streams and corecursion

  val ones: Stream[Int] = Stream.cons(1, ones)

  //Exercise 5.8
  //Generalize ones slightly to the function constant, which returns an infinite Stream of a given
  //value
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //Exercise 5.9
  //Write a function that generates an infinite stream of integers, starting from n, then n + 1,
  //n + 2, and so on.
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //Exercise 5.10
  //Write a function fibs that generates the infinite stream of Fibonacci numbers: 0,1,1,2,3,5,8,
  //and so on.
  def fibs: Stream[Int] = {
    def fibs(first: Int, second: Int): Stream[Int] = 
      Stream.cons(first + second , fibs(second, (first + second)))
    Stream.cons(0, Stream.cons(1, fibs(0,1)))
  }

  //Exercise 5.11
  //Write a more general stream-building function called unfold. It takes an initial state, and a 
  //function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  //The unfold function is an example of what's sometimes called a corecursive function. Whereas a 
  //recursive function consumes data, a corecursive function produces data. And whereas recursive
  //functions terminate by recursing on smaller inputs, corecursive functions need not terminate
  //so long as they remain productive, which just means that we can always evaluate more of the
  //result in a finite amount of time. Corecursion is also sometimes called guarded recursion, and
  //productivity is also sometimes called cotermination.

  //Exercise 5.12
  //Write fibs, from, constant, and ones in terms of unfold
  def fibs_2: Stream[Int] =
    unfold((0,1))((num: Tuple2[Int, Int]) => Some((num._1, (num._2, num._1 + num._2))))

  def from_2(n: Int): Stream[Int] = 
    unfold(n)((num: Int) => Some((num, num + 1)))

  def constant_2[A](a: A): Stream[A] = 
    unfold(a)((e: A) => Some((e, e)))

  def ones_2: Stream[Int] = 
    unfold(1)((Int) => Some((1, 1)))
}

object Test {
  def main(args: Array[String]) = {
    val str1 = Stream(1,2,3)
    val emptyStr = Stream()
    val infiniteStr = constant(2)
    val stepStr = from(1)

    runTest("forAll test less than 4", true,  str1.forAll(_ < 4))
    runTest("forAll test greater than 4", false, str1.forAll(_ > 4))

    runTest("Stream(1,2,3) headOption_2 is 1", 1, str1.headOption_2.get)
    runTest("Empty stream headOption_2 is None", None, emptyStr.headOption_2)

    runTest("First four elements of infinite stream", infiniteStr.take(4).toList, List(2,2,2,2))
    
    runTest("First four elements of stepping stream", stepStr.take(4).toList, List(1,2,3,4))

    runTest("First seven element of fibonacci stream", fibs.take(7).toList, List(0,1,1,2,3,5,8))


    runTest("First seven elements of ones stream using unfold", ones_2.take(7).toList, List(1,1,1,1,1,1,1))

    runTest("First seven elements of fibonacci stream using unfold", fibs_2.take(7).toList, List(0,1,1,2,3,5,8))

    runTest("First seven elements of from stream (starting at 1) using unfold", from_2(1).take(7).toList, List(1,2,3,4,5,6,7))
  }

  def runTest(testName: String, result: Any, expected: Any) {
    if(result == expected) 
      println("PASS")
    else 
      println("FAIL: " + testName + ". Expected: " + expected + " Result: " + result)
  }
}
