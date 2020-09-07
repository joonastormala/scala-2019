package o1

import constants._

class Game {
  
  val bug = new Bug(new Pos(100, 40))
 
  val obstacles = Vector(new Obstacle(70), new Obstacle(30), new Obstacle(20))
  
  def timePasses() = {
    bug.fall()
    obstacles.foreach(_.approach())
  }
  
  def activateBug() = bug.flap(15)


  def isLost = obstacles.exists(_.touches(this.bug)) || !this.bug.isInBounds
}
