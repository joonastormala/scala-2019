package o1

import scala.util.Random

// This class is gradually developed between Chapters 2.4 and 3.4.

class Odds(val wont: Int, val will: Int) {

  def probability = 1.0 * this.will / (this.wont + this.will)
  
  def fractional = (this.wont + "/" + this.will)
 
  def decimal = (this.wont + this.will).toDouble / this.will
  
  def winnings(bet: Double) = ((this.wont + this.will).toDouble / this.will) * bet
  
  def not = new Odds(this.will, this.wont)
  
  override def toString = fractional
  
  def both(another: Odds) = {
    val a = (this.wont * another.wont + this.wont * another. will + this.will * another.wont)
    val b = this.will * another.will
    val c = new Odds(a,b)
    c    
  }
  def either(another: Odds) = {
    val a = this.wont * another.wont
    val b = this.wont * another.will + this.will * another.wont + this.will * another.will
    val c = new Odds(a,b)
    c
  }
  def isLikely = this.wont < this.will
  
  def isLikelierThan(another: Odds) = this.probability > another.probability
  
  def moneyline = {
    if (this.probability < 0.50)
      (100 * this.wont / this.will).toInt
    else
      (-100 * this.will / this.wont).toInt
  }

}
