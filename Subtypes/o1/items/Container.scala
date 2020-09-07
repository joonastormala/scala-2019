package o1.items

import scala.collection.mutable.Buffer

class Container(name: String) extends Item(name) {
  private val contents = Buffer[Item]()
  override def toString = name + " containing " + contents.size + " item(s)"
  

  def addContent(newContent: Item): Unit = {
    contents += newContent
  }




}