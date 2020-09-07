package o1.people

class Passenger(val name: String, val card: Option[TravelCard]) {

  def canTravel: Boolean = {
    card match {
      case Some(id) => id.isValid
      case None => false
    }
  }
  
  def hasCard: Boolean = card.isDefined
    
    
}