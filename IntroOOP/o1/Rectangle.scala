package o1

// This class is introduced in Chapter 2.4.

class Rectangle(givenSideLength: Double, anotherGivenSideLength: Double) {

  val side1 = givenSideLength
  val side2 = anotherGivenSideLength

  def area = this.side1 * this.side2

  // Etc. You may write additional methods here.

}
