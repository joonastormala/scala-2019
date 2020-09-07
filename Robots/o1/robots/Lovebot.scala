package o1.robots

import o1._

class Lovebot(name: String, body: RobotBody, val beloved: RobotBody) extends RobotBrain(name, body){
  def moveBody() = {
    if(!(body.location.distance(beloved.location) == 1)) {
      if (body.location.xDiff(beloved.location).abs >= body.location.yDiff(beloved.location).abs) {
        body.moveTowards(body.location.xDirectionOf(beloved.location).getOrElse(body.facing))
      } else {
        body.moveTowards(body.location.yDirectionOf(beloved.location).getOrElse(body.facing))
      }
    }
      
  }
}


