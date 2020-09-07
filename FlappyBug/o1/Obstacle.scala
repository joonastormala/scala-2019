package o1

import constants._

import scala.util.Random

// This class is introduced in Chapter 2.6.

class Obstacle(val radius: Int) {

   private var currentPos = randomLaunchPosition()
   
   def pos = this.currentPos
  
  def approach() = {
    if (isActive)
      this.currentPos = this.pos.addX(-ObstacleSpeed)
    else 
      this.currentPos = randomLaunchPosition()
  }
  def touches(bugtouch: Bug) = {
    (bugtouch.pos).distance(this.pos) < BugRadius + this.radius
  }
  def isActive = (this.currentPos.xDiff(Pos((0 - this.radius), this.currentPos.y)) <= 0)

  private def randomLaunchPosition() = {
  val launchX = 1000 + this.radius + Random.nextInt(500)
  val launchY = Random.nextInt(400)
  new Pos(launchX, launchY)
  }
  override def toString = "center at " + this.pos + ", radius " + this.radius

}
