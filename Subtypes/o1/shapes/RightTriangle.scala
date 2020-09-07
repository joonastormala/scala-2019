package o1.shapes
import scala.math._

class RightTriangle(f: Double, s: Double) extends Shape {
  def hypotenuse = hypot(f,s)
  def area = (f * s) / 2
  def perimeter = f + s + hypot(f,s)
  
}