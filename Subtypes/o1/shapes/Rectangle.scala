package o1.shapes

// This class is introduced in Chapter 7.2.

class Rectangle(val sideLength: Double, val anotherSideLength: Double) extends Shape {

  def area = this.sideLength * this.anotherSideLength
  
  def perimeter = 2 * sideLength + 2 * anotherSideLength

}

