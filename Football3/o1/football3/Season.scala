package o1.football3

import scala.collection.mutable.Buffer
import scala.math._

class Season(){

  private val matches = Buffer[Match]()
  private var biggestW: Option[Match] = None
  private var newestMatch: Option[Match] = None
  
  def addResult(newResult: Match): Unit = {
    this.matches += newResult
    newestMatch = Some(newResult)
    this.biggestW match {
      case None =>
        this.biggestW = Some(newResult)
      case Some(oldBiggestW) =>
        val newBiggestW = if (abs((newResult.goalDifference)) > abs((oldBiggestW.goalDifference))) newResult else oldBiggestW
        this.biggestW = Some(newBiggestW)
    }
  }
  
  def biggestWin: Option[Match] = biggestW
  
  def latestMatch: Option[Match] = newestMatch
  
  def matchNumber(number: Int): Option[Match] = {
    if (matches.lift(number).isDefined)
      matches.lift(number)
    else None
  }
  
  def numberOfMatches: Int = matches.size
}
