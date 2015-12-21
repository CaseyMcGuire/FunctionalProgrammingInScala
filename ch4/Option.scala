//Exceptions are side effects and side effects are bad. So what is the alternative?
//The big idea is that we can represent failures and exceptions with ordinary values
//In this chapter, we will reimplement two standard Scala library types: Option and Either

object Example {
  def FailingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }//In Scala, a catch block is just a pattern matching block
  }
}
//Why are exceptions bad?
//1) Exceptions break referential transparency and introduce context dependence
//2) Exceptions are not typesafe


//We need a way to defer the decision of how to handle undefined cases so that they can be dealt with at
// the most appropriate level.

//The solution is to represent explicitly in the return type that a function may not always have an
//answer.

//Option has two cases: it can be defined in which case it will be Some, or it can be undefined in which
//case it will be None.

//Also note that here we are defining our Option functions inside the body of the Option trait, as
//opposed to in the companion object as we did with List. This means we can do obj.fn(arg1) or
//obj fn arg instead of fn(obj,arg1). This is a stylistic choice with no real significance, and we'll
//use both styles throughout the book.
sealed trait Option[+A] {

  //Exercise 4.1
  //Apply f if the Option is not None
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  //Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match {
      case None => None
      case Some(x) => f(x)
    }

  def flatMap2[B](f: A => Option[B]): Option[B] = 
    map(f).getOrElse(None)

  //The default: => B type annotation means the argument is of type B but won't be evaluated until it's
  //needed by the function.
  //THe B >: A type parameter indicates that B must be equal to or a supertype of A. It's needed to
  //convince Scala that it's still safe to declare Option[+A] as covariant in A.
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }

  //Returns the first option if it's defined; otherwise returns the second Option.
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(x => Some(x)) getOrElse ob
  }

  //Convert Some to None if the value doesn't satisfy f.
  def filter(f: A => Boolean): Option[A] = {
    if(map(f).getOrElse(false)) this
    else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  //Exercise 4.2
  //Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is
  //the mean of math.pow(x - m, 2) for each element x in the sequence. 
  //Note: Seq is the common interface of various linear sequence-like collections.
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  //The general rule of thumb is that we use exceptions only if no reasonable program would ever catch 
  //the exception; if for some callers the exception might be a recoverable error, we use Option to give
  //them flexibility

  //It may be easy to jump to the conclusion that once we start using Option, it infects our entire code 
  //base. But this doesn't happen and the reason is taht we can *lift* ordinary functions to become
  //functions that operate on Option.

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  //This tells us that any function that we already have lying around can be transformed (via lift) to 
  //operate within the context of a single Option value.

  //example)
  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  //The => A means that a is evaluated lazily
  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e: Exception => None }

  //The Try function is a general purpose function we can use to convert from an exception-based API to
  //an Option-oriented API. 

  //Exercise 4.3
  //Write a generic function map2 that combines two Option values using a binary function. If either
  //Option value is None, then the return value is too. 
  //My solution
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    (a,b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(x), Some(y)) => Some(f(x,y))
    }

  //This is the book's solution
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    a flatMap (aa => b map (bb => f(aa,bb)))

  def map2_3[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)

  //Exercise 4.4
  //Write a function sequence that combines a list of Options into one Option containing a list of
  //all the Some values in the original list. If the original list contains None even once, the
  //result of the function should be None; otherwise the result should be Some with a list of all
  //the values.
  //My solutions
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def sequence(a: List[Option[A]], acc: List[A]): Option[List[A]] =
      a match {
        case Nil => Some(acc.reverse)
        case Some(x)::xs => sequence(xs, x::acc)
        case None::_ => None
      }
    sequence(a, Nil);
  }

  //Note that here and in the book's solution, Scala incorrectly infers the type as Some[Nil.type]
  //and thus explicit type annotations must be added.
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((elem, acc) => 
      (elem, acc) match {
        case (None,_) => None
        case (_,None) => None
        case (Some(x), Some(xs)) => Some(x::xs)
      })

  //book's solutions
  def sequence3[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def sequence4[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))


  //Sometimes we'll want to map over a list using a function that might fail, returning None if 
  //applying it to any element of the list returns None.
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a match {
      case Nil => Some(Nil)
      case x::xs => f(x) flatMap(n => traverse(xs)(f) flatMap(m => Some(n::m)))
    }
}
