package o1.robots

import o1.CompassDir
import scala.util.Random

class Staggerbot(name: String, body: RobotBody, randomSeed: Int) extends RobotBrain(name, body) {
  
  private val dir = Vector(CompassDir.North, CompassDir.East, CompassDir.South, CompassDir.West)
  private val seed = new Random(randomSeed)
  
  private def randomizeDirection: CompassDir = {    
    dir(seed.nextInt(4))
  }
  
  def moveBody() = {
    if (body.moveTowards(randomizeDirection)) {
      body.spinTowards(randomizeDirection)
    }
    
  }
}
