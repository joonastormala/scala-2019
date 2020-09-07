package o1.robots

import o1._

class Psychobot(name: String, body: RobotBody) extends RobotBrain(name, body) {
  private val dir = Vector(CompassDir.North, CompassDir.East, CompassDir.South, CompassDir.West)
  
  private def checkIR(pos: GridPos): Boolean = {
    !body.world.apply(pos).robot.isEmpty && body.world.apply(pos).robot.get.isMobile
  }
    
  private def checkDir(direction: CompassDir) = {
    var loc = body.location
    var isRobot = false    
    do {
      loc = loc.neighbor(direction)
      if(checkIR(loc)) {
        isRobot = true
        body.spinTowards(direction)
        while (body.moveTowards(direction)) body.moveTowards(direction)
      }
        
    } while(!body.world.elementAt(loc).isUnpassable && !isRobot && body.world.elementAt(loc).robot.isEmpty)
        isRobot
  }
    
  def moveBody() = {
    var count = 0
    while(count<4 && !checkDir(dir(count))){
      count += 1
    }
    

  }
}

/*	var isRobot = false
	  val allrobots = body.location.pathTowards(direction).takeWhile(n => !body.world.elementAt(n).isUnpassable).filter(checkIR(_)).toVector 
    val spaceBetween = body.location.pathTowards(direction).takeWhile(n => !body.world.elementAt(n).isUnpassable && body.world.elementAt(n).isEmpty).toVector
    val temp = (spaceBetween.takeRight(1))(0)
    if(allrobots.size > 0) {
      isRobot = true
      body.spinTowards(direction)
      while (body.moveTowards(direction)) body.moveTowards(direction)
    }
    isRobot */

    /*
    var isRobot = false
    def allEmpty = body.location.pathTowards(direction).takeWhile(!body.world.elementAt(_).isUnpassable) //.foreach(println) // && body.world.elementAt(n).isEmpty
    val roboto = allEmpty.filterNot(body.world.elementAt(_).isEmpty)
    if(body.world.elementAt(roboto(0)).robot.get.isMobile)
      println("found robot")
    
    
    //val roboto = allEmpty.filterNot(body.world.elementAt(_).robot.get.isMobile)
    
    //if(contains robot)
        //move to robots location and crash to it*/

    /*for (i <- dir)
      checkDir(i) //find(body.world.elementAt(_).robot.get.isBroken) //!body.world.elementAt(n).robot.get.isBroken.foreach(println)
    
    //suora line eteenpäin, pysähtyy kun tulee vastaan seinä tai robootti joka on immobile. sisältääkö robootin?*/