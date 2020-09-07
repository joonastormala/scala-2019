package comb

import scala.io.StdIn._
import scala.util.Random

object Combat extends App{
  
  class Item(name: String)
  
  case class Boss (
    var hp: Int,
    baseDamage: Int = 25)
    
  case class Player (
    var hp: Int,
    baseDamage: Int = 10,
    Items: Seq[Item] = Nil)

  val boss = Boss(50)
  val player = Player(100)
  while (boss.hp > 0) {  
    var a = readLine("what do? \n")
    if (a == "attack") {
      var x = Random.nextInt(player.baseDamage)
      boss.hp -= x
      if (!(boss.hp < 0))
        if (x != 0) println(s"you swing att the boss and deal $x damage, it now has ${boss.hp} health left") else println("You miss!")
      } else {
        println("unknown command")
      }
  }
  println("you defeated the boss! great job")    
}