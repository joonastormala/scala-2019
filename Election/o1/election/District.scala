package o1.election
import scala.collection.mutable.Buffer


class District(val name: String, val seats: Int, val candidates: Vector[Candidate]) {
  
  override def toString: String = name + ": " + candidates.size + " candidates, " + seats + " seats"

  def printCandidates(): Unit = candidates.foreach( n => println(n.toString))

  def candidatesFrom(party: String) : Vector[Candidate] = candidates.filter(_.party == party)
  
  def topCandidate: Candidate = {
    var mostVoted = candidates(0)
    for (candidate <- candidates) {
      if(candidate.votes > mostVoted.votes) {
        mostVoted = candidate
      }        
    }
    mostVoted  
  }
  
  def totalVotes(party: String): Int = {
    /*
    var totalSoFar = 0
    
    for (candidate <- candidates) {
      if (candidate.party == party) {
        totalSoFar += candidate.votes
      }
    }
    totalSoFar
    */
    val candidate = candidates.filter(_.party == party)
    candidate.foldLeft(0)(_ + _.votes)
    
    
  }
  
  def totalVotes: Int = {
    /*
    var totalV = 0
    for (candidate <- candidates) {
      totalV += candidate.votes
    }
    totalV*/
    
    candidates.foldLeft(0)(_ + _.votes)
    
  }
  
}
