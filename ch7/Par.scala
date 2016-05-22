package ch7

import java.util.concurrent._
import java.util.concurrent.atomic._


object Par {
  type Par[A] = ExecutorService => Future[A]

  //For taking an unevaluated A and returning a computation that might evaluate it in a separate
  //thread. We call it unit because in a sense it creates a unit of parallelism that just wraps
  //a single value.
  def unit[A](a: => A): Par[A] = {
    (es: ExecutorService) => UnitFuture(a)
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //Fully evaluates a given Par, spawning parallel computations as requested by fork and extracting 
  //the resulting value
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)



  //Marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      override def call = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //Combines the results of two parallel computations with a binary function
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    (es: ExecutorService) => { 
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) 
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _) 
    }

  //Exercise 7.4
  //Write a function to convert any function A => B to one that evaluates its result asynchronously
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a: A => lazyUnit(f(a))
  }

  //Exercise 7.5
  //Write this function, called sequence. 
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    es => unit(ps.map(x => x(es).get))(es)


  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //Exercise 7.6
  //Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = { 

    def go(lst: List[A], a: Par[A] => Future[A]): List[A] = {
      lst match {
        case x::xs => {
          val computation: Future[A] = a(unit(x))
          val restOfList: List[A] = go(xs, a)
          val computationResult: A = computation.get
          if (f(computationResult)) {
            computationResult::restOfList
          }
          else {
            restOfList
          }
        }
        case Nil => Nil
      }
    }

    es => { 
      val filteredList = go(as, run(es)(_))
      unit(filteredList)(es)
    }
  }

}

/*
 * We'll introduce our own version of Future with which we can register a callback that will be invoked when the
 * result is ready.
 */
object Nonblocking {
  object Par {
    sealed trait Future[+A] {
      //Note: What this would be saying is that this method is private to the parallelism package.
      /* private[parallelism] */
      private[ch7] def apply(k: A => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown }
      latch.await
      ref.get
    }

    def unit[A](a: A): Par[A] = 
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = 
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] = 
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = 
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit = {
      es.submit(new Callable[Unit] { def call = r })
    }

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = 
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None 
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a,b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a,b)))
            }
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }
  }
}
