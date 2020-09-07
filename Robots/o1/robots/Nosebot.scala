package o1.robots

import o1._

class Nosebot(name: String, body: RobotBody) extends RobotBrain(name, body){
  
  def moveBody(): Unit = {
    var canMove = false
    var count = 0
      do {
        canMove = attemptMove()
        count += 1
      } while (canMove == false && count < 4)
        
  }
  
  def attemptMove(): Boolean = {
    if(moveCarefully()) {
      true      
    } else {
      body.spinClockwise()
      false
    }
  }
}


