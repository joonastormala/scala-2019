package o1.family

object FamilyTest extends App {
  val Edith = new FamilyMember("Edith")
  val Milton = new FamilyMember("Milton")
  val Lewis = new FamilyMember("Lewis")
  val Dawn = new FamilyMember("Dawn", Vector(Lewis, Milton, Edith))
  val Gus = new FamilyMember("Gus")
  val Greg = new FamilyMember("Greg")
  val Molly = new FamilyMember("Molly")
  val Barbara = new FamilyMember("Barbara")
  val Calvin = new FamilyMember("Calvin")
  val Walter = new FamilyMember("Walter")
  val Sam = new FamilyMember("Sam", Vector(Dawn, Gus, Greg))
  val Edie = new FamilyMember("Edie", Vector(Molly, Barbara, Calvin, Sam, Walter))
  val Odin = new FamilyMember("Odin", Vector(Edie))
  println(Odin.numberOfDescendants)
  println(Edie.mostChildren)
  println(Odin.numberOfDescendingGenerations)
  println(Odin.everyoneBelow)
  //Odin.everyoneBelow
}