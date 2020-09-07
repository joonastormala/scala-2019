package game

class Monster(val name: String, var hp: Int, val damage: Int) {
  
  def isAlive = hp > 0
  
  
}