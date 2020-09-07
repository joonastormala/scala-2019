package o1

import constants._

class Bug(private var currentPos: Pos) {

  val radius = BugRadius
  private var yVelocity = 0.0
  
  def pos = currentPos
  
  def flap(changeY: Double) = {
    yVelocity = -changeY
  }
    
  def fall() =  {
    if (currentPos != Pos(this.pos.x, 350))
    yVelocity = yVelocity + 2
    move(yVelocity)
  }
  
  def isInBounds = (this.currentPos.y > 0 && this.currentPos.y < 350 )

  
  override def toString = "center at " + this.pos + ", radius " + this.radius
  
  def move(arvo: Double) = {   
    this.currentPos = this.pos + Pos(0, arvo)
    this.currentPos = this.pos.clampY(0, 350)
  }
    
  
}