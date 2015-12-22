
//One thing you may have noticed is that it doesn't tell us anything about what went wrong in the case
//of an exceptional condition

//We can craft a data type that encodes whatever information we want about failures. The Either data
//type lets us track a reason for the failure.

//Either has only two cases, just like Option. The essential difference is that both cases carry a 
//value. The Either data type represents in a very general way, values that can be one of two things.
//We can say that it's disjoint union of two types. When we use it to indicate success or failure, by
//convention the Right constructor is reserved for the success case and Left is used for failure.

sealed trait Either[+E, +A] {
  //Exercise 4.6
  //implement the following functions
  def map[B](f: A => B): Either[E,B] = 
    this match {
      case Left(elem) => Left(elem)
      case Right(elem) => Right(f(elem))
    }

  //Again, remember that EE >: E means EE must be equal to or a supertype of E.
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE,B] = 
    this match {
      case Left(elem) => Left(elem)
      case Right(elem) => f(elem)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(elem) => Right(elem)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    this flatMap (a => b map (bb => f(a,bb)))

  //book's answer
  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = 
    for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  //let's look at an example
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if(xs.isEmpty) 
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)

  //sometimes we might want to include more information about the error, for example a stack trace
  //showing the location of the error in the source code. In such cases, we can simply return 
  //the exception in the Left side of an Either:
  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  //as we did with option, we can write a function, Try, which factors out this common pattern of 
  //converting thrown exceptions to values: 
  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    es match {
      case Nil => Right(Nil)
      case x::xs => x flatMap (xx => sequence(xs) flatMap (xxs => Right(xx::xxs)))
    }


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as match {
      case Nil => Right(Nil)
      case x::xs => f(x) flatMap (n => traverse(xs)(f) flatMap(m => Right(n::m)))
    }

}


//example
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int) 

object Person {
  def mkName(name: String): Either[String, Name] =
    if(name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if(age < 0) Left("Age is out of range")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))
}
