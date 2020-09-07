package o1.people

class Member(val id: Int, val name: String, val yearOfBirth: Int, val yearOfDeath: Option[Int]) {

  def isAlive: Boolean = !yearOfDeath.isDefined
  
  override def toString: String = {
    if(isAlive){
      name + "(" + yearOfBirth + "-" + ")"
    }
    else {
      yearOfDeath match {
      case Some(year) => name + "(" + yearOfBirth + "-" + year + ")"
      case None => name + "(" + yearOfBirth + "-" + ")"
       
      }
    }
  }
    

}
