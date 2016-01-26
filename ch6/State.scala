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
    rng.nextInt
  }
}
