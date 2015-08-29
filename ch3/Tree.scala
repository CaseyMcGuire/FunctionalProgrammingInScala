
//List is just one example of what's called an algebraic data type. An ADT is just a data type defined by
// one or more data constructors, each of which may contain zero or more arguments. We say that the data
//type is the sum or union of its data constructors, and each data constructor is the product of its 
//arguments

//Functional Binary Trees
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //Exercise 3.25
  //Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](tree: Tree[A]): Int = 
    tree match {
      case Branch(x,y) => size(x) + size(y)
      case Leaf(_) => 1
    }


  //Exercise 3.26
  //Write a function maximum that returns the maximum element in a Tree[Int]
  def maximum(tree: Tree[Int]): Int = 
    tree match {
      case Branch(x,y) => maximum(x) max maximum(y)
      case Leaf(x) => x
    }


  //Exercise 3.27
  //Write a function depth that returns the maximum path length from the root of
  //a tree to any leaf
  def depth[A](tree: Tree[A]): Int = {
    def depth[A](acc: Int, tree: Tree[A]): Int = {
      val newAcc = acc + 1
      tree match {
        case Branch(x,y) => depth(newAcc, x) max depth(newAcc, y)
        case Leaf(_) => newAcc
      }
    }
    depth(0,tree)
  }

  //Exercise 3.28
  //Write a function map that modifies each element in a tree with a given
  //function
  def map[A,B](tree: Tree[A], f: A => B): Tree[B] = 
    tree match {
      case Branch(x,y) => Branch(map(x,f), map(y,f))
      case Leaf(x) => Leaf(f(x))
    }

  //Exercise 3.29
  //Write a function fold that abstracts over the similarities of size, maximum,
  //depth, and map.
  def fold[A,B](as: Tree[A], z: B)(f: (A,B) => B): B = {

  }

}
