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

  //Exercise 4.2
  //Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is
  //the mean of math.pow(x - m, 2) for each element x in the sequence. 
  //Note: Seq is the common interface of various linear sequence-like collections.
  def variance(xs: Seq[Double]): Option[Double] = {
    
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else xs.sum / xs.length
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


