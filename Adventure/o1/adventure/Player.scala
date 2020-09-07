package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val inv = Map[String, Item]()


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

  /** Tries to drop an item of the given name. This is successful if such an item is currently in the player's possession.
   *  If so, the item is removed from the player's inventory and placed in the area.
   *  Returns a description of the result of the attempt: "You drop the ITEM." or "You don't have that!". */
  def drop(itemName: String): String = {
    if(inv.contains(itemName)) {
      this.location.addItem(inv(itemName))
      inv -= itemName      
      "You drop the " + itemName + "."
    } else {
      "You don't have that!"
    }
  }
  
  /** Causes the player to examine the item of the given name. This is successful if such an item is currently in the player's possession.
   *  Returns a description of the result, which, if the attempt is successful, includes a description of the item.
   *  The description has the form: "You look closely at the ITEM.\nDESCRIPTION" or "If you want to examine something, you need to pick it up first." */
  def examine(itemName: String): String = if (has(itemName)) "You look closely at the " + itemName + ".\n" + inv(itemName).description else "If you want to examine something, you need to pick it up first."

  /** Tries to pick up an item of the given name. This is successful if such an item is located in the player's current location. 
   *  If so, the item is added to the player's inventory. 
   *  Returns a description of the result: "You pick up the ITEM." or "There is no ITEM here to pick up." */
  def get(itemName: String): String = {
    if (location.contains(itemName)){
      inv += itemName -> location.removeItem(itemName).get
      "You pick up the " + itemName + "."
    } else {
      "There is no " + itemName + " here to pick up."
    }
  }
  
  /** Determines whether the player is carrying an item of the given name. */
  def has(itemName: String): Boolean = inv.contains(itemName)
  
  /** Causes the player to list what they are carrying. Returns a listing of the player's possessions or a statement indicating that the player is carrying nothing. 
   *  The return value has the form "You are carrying\nITEMS ON SEPARATE LINES" or "You are empty-handed." The items are listed in an arbitrary order. */
  def inventory: String = {
    if(inv.isEmpty) {
      "You are empty-handed."
    } else {
      "You are carrying\n" + inv.keys.mkString("\n")
    }
  }
  
  

}


