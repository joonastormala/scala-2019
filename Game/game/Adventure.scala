package game


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "A Dungeon Adventure"
  
  private var start = new Area("Great Hall", "This place is Huge! You can get lost in here easily")
  private val r2 = new Area("Hallway","The hallway is decorated with old paintings")
  private val r3 = new Area("Small storage room","It's a dead end, Maybe I should head back")
  private val r4 = new Area("Jailroom","It smells like excrement in here. Eww...")
  private val r5 = new Area("Abandoned Library","Nobody has been here in ages. You can smell mold and rotting books")
  private val r6 = new Area("Secret Passage","This Room wasn't always here...")
  private val r7 = new Area("Observatory", "There's something magical about the area.\nMaybe this could be the way out...")
  private val destination = r7
  
  
  start.setNeighbors(Vector("north" -> r2, "south" -> r5))
     r2.setNeighbors(Vector("north"-> r3, "south" -> start, "west" -> r4))
     r3.setNeighbors(Vector("south" -> r2))
     r4.setNeighbors(Vector("east" -> r2))
     r5.setNeighbors(Vector("north" -> start))
     r6.setNeighbors(Vector("east" -> r7, "west" -> start))
     r7.setNeighbors(Vector("west" -> r6))
  
  val ghost = new Monster("Spooky Ghost", 30, 20)
  r5.addMonster(ghost)
         
  //place these two items in clearing and southForest, respectively
  //r4.addItem(new Item("key", "It's a magical key"))
  r4.addItem(new Item("orb", "It's a magical orb used for teleportation."))
  r3.addItem(new Item("pills", "I wonder what these will do"))

  /** The character that the player controls in the game. */
  val player = new Player(start)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 40


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination && this.player.hasUsedItem

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.player.health <= 0

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You find yourself in a Dungeon. Defeat enemies and escape!\n\nBetter hurry, you can hear the guards' steps echoing from afar"

  
  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You feel the orb vibrate and suddenly you see a flash of light!\nYou have escaped!"
    else if (this.turnCount == this.timeLimit)
      "The guards have surrounded you, you failed to escape!\nGame Over"
    else if (this.player.health <= 0)
      "You Died. Game Over"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {   
    if(!r5.hasMonsters){ //if room 5 has been cleared of monsters, opens up a new area.
      start.setNeighbors(Vector("north" -> r2, "east" -> r6, "south" -> r5))
    }    
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")

  }


}

