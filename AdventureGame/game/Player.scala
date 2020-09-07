package game

import scala.collection.mutable.Map
import scala.util.Random

/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val inv = Map[String, Item]()
  private var itemUsedB = false
  private var hp = 30                               // Values for PvE
  private var hpMax = 30
  private var hitModifier = 10
    
  def health = hp
  
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven
  
  /** Checks if the player has used the item required for winning the game.  */
  def hasUsedItem = this.itemUsedB
  
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
  
  /** Causes the player to rest for a short while.
    * Returns a description of what happened. */
  def rest() = {
    if (this.location.hasMonsters) {
      "You cant rest while theres enemies nearby"
    } else {
    hp = hpMax
    "You rest for a while and recover some health."
    }
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
  
  /** Uses item if it's in inventory*/
  def use(itemName: String) = {
    if(inv.contains(itemName)){
      itemName match {
        case "pills" => hitModifier += 10; inv.remove("pills"); "You eat the magical pills and feel stronger!"
        case "orb" => itemUsedB = true; "You rub the orb, it starts to shimmer."
      }
    } else
      "You don't have that!"
  }
  /** PvE function. player and enemy makes attacks. Randomizes damage for enemy and player. calculates hp after turn */
  def attack() = {    
    if (this.location.hasMonsters) {      
      var monsterName = this.location.monsters.head.name
      var randomHit = Random.nextInt(hitModifier)
      var enemyHit = Random.nextInt(this.location.monsters.head.damage)
      var toBeHp = this.location.monsters.head.hp - randomHit
      var playerHp = hp - enemyHit
      if (toBeHp > 0 && playerHp > 0) { // Both parties are alive
          this.location.monsters.head.hp = this.location.monsters.head.hp - randomHit
          hp -= enemyHit
          s"You attack the ${monsterName} and deal $randomHit damage, it has ${this.location.monsters.head.hp} health left" + 
          s"\nThe $monsterName swings at you and deals $enemyHit damage! You have $hp HP left"        
      } else if (playerHp <= 0) { // Player Dies
          hp -= enemyHit
          s"The $monsterName swings at you.\nYou collapse..."
      } else { // Enemy Dies
          this.location.removeMonster()
          s"You kill the ${monsterName}!\n\nYou hear sound of something opening in the Great Hall"
      }
    } else { // No enemies
      "Theres nothing to attack here"
    }
  }
  
  def help() = {
    "Commands: " +     
    "\n- examine[itemname]:  Causes the player to examine the item of the given name." +
    "\n- drop [itemname]:    Tries to drop an item of the given name." +
    "\n- use [itemname]:     Uses item if you have it in your inventoryn" +
    "\n- get [itemname]:     Tries to pick up an item of the given name." +    
    "\n- go [direction]:     Attempts to move the player in the given direction." +
    "\n- inventory:          Causes the player to list what they are carrying." +
    "\n- rest:               You rest for a while and recover lost health" +
    "\n- attack:             Attacks nearby monster if theres any" +
    "\n- hp:                 Displays your current health" +
    "\n- quit:               Exits the game"
  }
  

  
  // Used for checking how much hp you have left
  def getHp() = s"You have $hp HP"
  
  def xyzzy = {
    hpMax = 999
    hp = 999
    hitModifier = 999
    "You feel like a God!"
  }
}

