
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

}
