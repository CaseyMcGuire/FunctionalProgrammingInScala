object Test {
  def main(args : Array[String]) {

    assert(List.init(List(1,2,3,4,5)) == List(1,2,3,4), "Init failed")
    
    //Exercise 3.8
    println(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))

    val fiveElemList: List[Int] = List(1,2,3,4,5)
    val nilList: List[Int] = Nil

    //Exercise 3.9
    assert(List.length(fiveElemList) == 5, "Expected list of length 5")
    assert(List.length(nilList) == 0, "Expected list of length 0")


    assert(List.length2(fiveElemList) == 5, "Expected list of length 5")
    assert(List.length2(nilList) == 0, "Expected list of length 0")


    assert(List.reverse(fiveElemList) == List(5,4,3,2,1))
    assert(List.reverse(nilList) == Nil)


    //3.14
    assert(List.append(fiveElemList, List(6)) == List(1,2,3,4,5,6))
    assert(List.append(nilList, List(1)) == List(1))
    assert(List.append(nilList, Nil) == Nil)

    
    assert(List.append2(fiveElemList, List(6)) == List(1,2,3,4,5,6))
    assert(List.append2(nilList, List(1)) == List(1))
    assert(List.append2(nilList, Nil) == Nil)

    //3.15
    assert(List.concat(List(List(1),List(2),List(3,4))) == List(1,2,3,4))
    assert(List.concat2(List(List(1),List(2),List(3,4))) == List(1,2,3,4))

    //3.20
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))

    assert(List.filter(List(1,2,3,4,5,6))(i => i > 4) == List(5,6))

   assert(List.merge(List(1,2,3),List(1,2,3)) == List(2,4,6))

    //3.24
    assert(List.hasSubsequence(List(1,2,3,4,5), List(4,5)))
  }
}
