package o1.football2

import scala.collection.mutable.Buffer


/** The class `Match` represents match results in a football match statistics program.
  * A match is played between teams from two clubs: a home club and an away club.
  * Goals scored by players of either team can be added to the match object with the
  * method `addGoal`.
  *
  * The class is expected to be used so that a match object with no goals is initially
  * created as a real-life match starts. Goals are added incrementally as the match
  * progresses. (A match object has mutable state.)
  *
  * @param home  the club whose team plays at home in the match
  * @param away  the club whose team plays away in the match */
class Match(val home: Club, val away: Club) {

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here


  def addGoal(scorer: Player): Unit = {
    if (scorer.employer == this.home) {
      this.homeScorers += scorer
    }
    else {
      this.awayScorers += scorer
    }

  }
  
  def allScorers: Vector[Player] =
    (homeScorers ++ awayScorers).toVector
  
  def hasScorer(possibleScorer: Player): Boolean = {
    if (possibleScorer.employer == this.home) {
      homeScorers.contains(possibleScorer)
      
    }
    else {
      awayScorers.contains(possibleScorer)
    }
  }
  
  def winningScorerName: String = {
    if (isHomeWin) {
      val scorer = homeScorers.drop(awayScorers.size)(0)
      scorer.name
    }
    else if (isAwayWin) {
      val scorer = awayScorers.drop(homeScorers.size)(0)
      scorer.name
      
    }
    else {
      "no winning goal"
    }
  }
      
  def homeGoals = homeScorers.size
  def awayGoals = awayScorers.size
  def totalGoals = this.homeGoals + this.awayGoals
  def isGoalless = !(this.totalGoals > 0)
  def isHomeWin = this.homeGoals > this.awayGoals
  def isAwayWin = this.homeGoals < this.awayGoals
  def isTied = this.homeGoals == this.awayGoals
  def isHigherScoringThan(anotherMatch: Match) = this.totalGoals > anotherMatch.totalGoals
  def location = home.stadium
  def goalDifference = {
    this.homeGoals - this.awayGoals
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

  def winnerName = {
    if (this.goalDifference < 0)
      this.away.name
    else if (this.goalDifference > 0)
      this.home.name
    else
      "no winner"
  }

}



