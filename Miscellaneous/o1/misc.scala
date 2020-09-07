package o1
object misc { // These definitions at the top are discussed in Chapter 5.2.

  // Various small assignments across several chapters will ask you to define functions in this file.
  // Please enter your code below this comment.
  def tempo(string: String) = {
    if (string.contains("/")){
      var a = string.split("/")
      a(1).toInt
    } else 120
  }
  
  def toMinsAndSecs(x: Int) = {
    (x / 60, x % 60)
  }

  def isAscending(vec: Vector[Int]): Boolean = {    
    vec.zip(vec.tail).forall{case (x,y) => x <= y} // {case(x,y) => isInOrder(x,y)}    
  }



  def isInOrder(pairOfNumbers: (Int, Int)) = pairOfNumbers._1 <= pairOfNumbers._2    // This example function is introduced in Chapter 8.4. You can ignore it until then.

}