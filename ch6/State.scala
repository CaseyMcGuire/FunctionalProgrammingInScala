//In this chapter, we'll see how to write purely functional programs that manipulate state, using
//the simple domain of random number generation.

//The goal here is to give you the basic pattern for how to make *any* stateful API purely functional

//To generate random numbers in Scala, we can use scala.util.Random
//rng.nextDouble -> 0.9867...
//rng.nextDouble -> 0.8455...

//Even if we didn't know anything about what happens inside scala.util.Random, we can assume that
//the object rng has some internal state that gets updated after each invocation, since we'd
//otherwise get the same value each time we called nextInt or nextDouble. 
//Because state updates are performed as a side effect, these methods aren't referentially
//transparent which in turn implies that they aren't as testable, composable, modular, and easily
//parallelizable as they could be.

//The key to recovering referential transparency is to make the state updates *explicit*. Don't update
//the state as a side-effect, but simply return the new state along with the value that we're
//generating. Here's on possible interface to a random number generator.
trait RNG {
  def nextInt: (Int, RNG)
}

//Rather than returning only the generated random number (as is done in scala.util.Random) and
//updating some internal state by *mutating* it in place, we return the random number and the new
//state, leaving the old state unmodified. In effect, we separate the concern of computing what the 
//next state is from the concern of communicating the new state to the rest of the program.
//No global mutable memory is being used - we simply return the next state to the caller.
//This leaves the caller of nextInt in complete control of what to do with the new state. Note that
//we're still encapsulating the state, in the sense that users of this API don't know anything about
//implementation of the RNG itself.

//But we do need some sort of implementation so let's choose one.
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

//This problem of making seemingly stateful APIs pure and its solution (having the API *compute* the 
//next state rather than actually mutate anything) aren't unique to random number generation. 

//For instance, suppose you have a class like this:
//class Foo {
//  private var s: FooState = ...
//  def bar: Bar
//  def baz: Int
//}

//Suppose baz and bar mutate s in some way. We can mechanically translate this to the purely
//functional API by making explicit the transition from one state to the next.

//trait Foo {
//  def bar: (Bar, Foo)
//  def baz: (Int, Foo)
//}

//You might notice that this can get tedious. 
object RNG {
//Exercise 6.1
//Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue
//inclusive. Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn't 
//have a non-negative counterpart
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, nextRng) = rng.nextInt
    if(num == Int.MinValue) (0, nextRng)
    else if(num < 0) (-(num + 1), nextRng)
    else             (num, nextRng)
  }


  //Exercise 6.2 
  //Write a function to generate a Double between 0 and 1, not including 1. Note: You can use
  //Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDoublee to
  //convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (nextNum, nextRNG) = nonNegativeInt(rng)
    (nextNum.toDouble / (Int.MaxValue.toDouble + 1), nextRNG)
  }


  //Exercise 6.3
  //Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val (nextDouble, nextRNG2) = double(nextRNG)
    ((nextInt, nextDouble), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((nextInt, nextDouble), nextRNG) = intDouble(rng)
    ((nextDouble, nextInt), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //Exercise 6.4
  //Write a function to generate a list of random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 to count).foldLeft((List[Int](), rng))((acc, _) => {
      val (list, curRNG) = acc
      val (nextInt, nextRNG) = rng.nextInt
      (nextInt::list, nextRNG)
    })
  }

  //Looking back at our implementations, we'll notice a common pattern: each of our functions has a type of the form RNG => (A, RNG)
  //for some type A. Functions of this type are called state actions or state transitions because they transform RNG states from one
  //to the next. These state actions can be combined using combinators, which are higher-order functions. Since it's pretty tedious 
  //and repetitive to pass the state along ourselves, we want our combinators to pass the state from one action to the next 
  //automatically.

  type Rand[+A] = RNG => (A, RNG)

  //We can think of a value of type Rand[A] as "a randomly generated A", although that not really precise. It's really a state action-
  //a program that depends on some RNG, uses it to generate an A, and also transitions the RNG to a new state.

  val int: Rand[Int] = _.nextInt

  //We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state. 

  //A simple RNG state transition is the unit action, which passes the RNG state through without using it, always returning a constant
  //value rather than a random value
  def unit[A](a: A): Rand[A] = 
    rng => (a, rng)

  //There's also map for transforming the output of a state action without modifying the state itself. Remember, Rand[A] is just a type
  //alias for a function type RNG => (A, RNG), so this just a kind of function composition: 
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i - i % 2)

  //Exercise 6.5 
  //Use map to reimplement double in a more elegant way.
  def double2: Rand[Double] = 
    map(nonNegativeInt)(x => (x.toDouble / (Int.MaxValue.toDouble + 1)))

  //we need a new combinator that can combine two RNG actions into one using a binary rather than unary function

  //Exercise 6.6
  //Write the implementation of map2 based on the following signature. This function takes two actions, ra and rb, and a function f for
  //combining their results, and returns a new action that combines them: 

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (elem1, rng1) = ra(rng)
      val (elem2, rng2) = rb(rng1)
      (f(elem1, elem2), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
    map2(ra, rb)((_,_))

  val randIntDouble: Rand[(Int, Double)] = 
    both(int, double)

  //Exercise 6.7
  //If you can combine two RNG transitions, you should be able to combine a whole list of them. Implement sequence for combining a List of 
  //transitions into a single transition. 
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val lst = 
        fs.map(f => {
          val (elem, _) = f(rng)
          elem
      })
      val (_, nextRNG) = rng.nextInt
      (lst, nextRNG)
    }
  }


  //Exercise 6.8
  //Implement flatMap, and then use it to implement nonNegativeLessThan
  //flatMap allows us to generate a random A with Rand[A], and then take that A and choose a Rand[B] based on its value. 
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (elem, nextRng) = f(rng)
      g(elem)(nextRng)
    }


  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        nextRng: RNG => (mod: Int, nextRng: RNG)
      } else {
        nonNegativeLessThan(i)
      }
    })

  //Reimplement map and map2 in terms of flatMap.
  def map_2[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s) { x => 
      unit(f(x))
    }

  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
    flatMap(ra)(x => {
      flatMap(rb)(y => {
        unit(f(x,y))
      })
    })


  //The functions we've written aren't really specific to random number generation at all. They're general purpose functions for working with state actions,
  //and don't care about the type of state. 

  //  type State[S, +A] = S => [A,S]

  //Here State is short for computation that carries some state along, or state action, state transition, or even statement.
  //Now you could write type Rand[A] = State[RNG, A]

}

case class State[S,+A](run: S => (A,S)) {
  
}

object State {

}

object Main {
  def main(args: Array[String]): Unit = {

  }
}
