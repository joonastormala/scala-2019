package o1.football3

import scala.collection.mutable.Buffer


class Match(val home: Club, val away: Club) {

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here

      
  def homeGoals   =   homeScorers.size
  def awayGoals   =   awayScorers.size
  def location    =   home.stadium
  def totalGoals  =   this.homeGoals + this.awayGoals
  def isGoalless  =   !(this.totalGoals > 0)
  def isHomeWin   =   this.homeGoals > this.awayGoals
  def isAwayWin   =   this.homeGoals < this.awayGoals
  def isTied      =   this.homeGoals == this.awayGoals  
  def goalDifference = this.homeGoals - this.awayGoals
  def isHigherScoringThan(anotherMatch: Match) = this.totalGoals > anotherMatch.totalGoals

  
  def addGoal(scorer: Player): Unit = {
    if (scorer.employer == this.home) {
      this.homeScorers += scorer
    } else this.awayScorers += scorer
  }
  
  def allScorers: Vector[Player] =
    (homeScorers ++ awayScorers).toVector
  
  def hasScorer(possibleScorer: Player): Boolean = {
    if (possibleScorer.employer == this.home) {
      homeScorers.contains(possibleScorer)      
    } else awayScorers.contains(possibleScorer)
  }

  def winningScorer: Option[Player] = {
    if (isHomeWin) {
      homeScorers.drop(awayScorers.size).lift(0)      
    }
    else if (isAwayWin) {
      awayScorers.drop(homeScorers.size).lift(0)            
    }
    else {
      None
    }
  }
  
  def winnerName: String = winner match {    
    case Some(winner) => winner.name
    case None         => "no winner"
  }

  def winner: Option[Club] = {
    if (isHomeWin) 
      Some(this.home)
    else if (isAwayWin)
      Some(this.away)
    else None
  }

  override def toString = {
    
    val print = home.name + " vs. " + away.name + " at " + home.stadium + ": "
    if (this.isGoalless) {
      print + "tied at nil-nil"
    }
    else if (this.isTied) {
      print + "tied at " + this.homeGoals +"-all"
    }
    else if (this.isHomeWin){
      print + this.homeGoals + "-" + this.awayGoals + " to " + home.name
    } else {
      print + this.awayGoals + "-" + this.homeGoals + " to " + away.name
    }      
  }        

  /*def winnerName = {
    if (this.goalDifference < 0)
      this.away.name
    else if (this.goalDifference > 0)
      this.home.name
    else "no winner"
  }*/


}



