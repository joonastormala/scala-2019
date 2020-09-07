package o1

import constants._

// This class is introduced in Chapter 2.7.

object FlappyBugApp extends App {
/*
  val sky        = rectangle(ViewWidth, ViewHeight,  LightBlue)
  val ground     = rectangle(ViewWidth, GroundDepth, SandyBrown)
  val trunk      = rectangle(30, 250, SaddleBrown)
  val foliage    = circle(200, ForestGreen)
  val tree       = trunk.onto(foliage, TopCenter, Center)
  val rootedTree = tree.onto(ground, BottomCenter, new Pos(ViewWidth / 2, 30))
  val scenery    = sky.place(rootedTree, BottomLeft, BottomLeft)
*/  
  def scenery = {

    val scenery = rectangle(ViewWidth, ViewHeight,  LightBlue)
        .place((((rectangle(30, 250, SaddleBrown))
        .onto((circle(200, ForestGreen)), TopCenter, Center))
        .onto((rectangle(ViewWidth, GroundDepth, SandyBrown)),
        BottomCenter, new Pos(ViewWidth / 2, 30))), BottomLeft, BottomLeft)
    scenery
  }


  val bugPic = Pic("ladybug.png")
  

  def rockPic(obstacle: Obstacle) = Pic("obstacle.png")scaleTo(obstacle.radius * 2)


  val newGame = new Game
  val gui = new View(newGame, "FlappyBug") {
    var background = scenery
    def makePic = {
      var theseObstacles = newGame.obstacles
      var scene = background.place(bugPic, newGame.bug.pos)
      theseObstacles.foldLeft(scene)((orig, next) => orig.place(rockPic(next), next.pos))      
      }
    override def onKeyDown(painettu: Key) = {
      if (painettu == Key.Space)
        newGame.activateBug()
      }
    override def onTick() = {
      newGame.timePasses()
      this.background = this.background.shiftLeft(BackgroundSpeed)
      
      }
    override def isDone = newGame.isLost
  }
  gui.start()





}

