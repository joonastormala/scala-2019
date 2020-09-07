package o1.robots.tribal
import scala.collection.mutable.Buffer

class Stack[Element] {


  private var stackVector: Buffer[Element] = Buffer()
  
  def push(newFrame: Element) = {
    stackVector = newFrame +: stackVector
  }
  
  def peek: Option[Element] = {
    stackVector.lift(0)
  }
  
  def pop() = {
    val store = stackVector.lift(0)
    stackVector = stackVector.drop(1)
    store
  }
  
  def size = stackVector.size
  
  override def toString = "A stack of size " + this.size 
  
}


