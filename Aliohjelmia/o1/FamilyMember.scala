package o1.family
import scala.math.max

/** The class `FamilyMember` represents people in a very simple family tree analysis program.  
  * Each person has a name and zero or more children. A `FamilyMember` object is immutable.
  *
  * '''Note: All the methods in this class assume that each descendant descends from
  * a person via a single line of ancestry only.'''
  *
  *
  * @param name      the person's name 
  * @param children  the person's children */
class FamilyMember(val name: String, val children: Vector[FamilyMember]) {
  
  /** Initializes a new, childless `FamilyMember`. */
  def this(name: String) = this(name, Vector())
  
  
  /** Returns the number of children the person has. */
  def numberOfChildren = this.children.size

  
  /** Returns `true` if and only if the person has no children. */
  def isChildless = this.numberOfChildren == 0
  
  
  /** Returns the number of generations below this person in the family tree. 
    * For a person with no children, this will be zero; for someone with at    
    * least one child but no grandchildren, this will be one; for someone with 
    * grandchildren but no greatgrandchildren, this will be two, and so on. */  
  def numberOfDescendingGenerations: Int = {
    
    def anotherOne(input: FamilyMember, genCount: Int): Int = {
      if(input.children.size == 0) genCount + 0
      else {
        val another = genCheck(input, 0, genCount) 
        max(genCount + 1, another)
      }
    }
    def genCheck(parent: FamilyMember, start: Int, genHigh: Int): Int = {
      if(parent.children.size == start + 1){
        anotherOne(parent.children(start), (genHigh + 1))
      } else {
        val highestOfOthers = genCheck(parent, start + 1, genHigh)
        max(anotherOne(parent.children(start), genHigh + 1), highestOfOthers)
      }
    }
    anotherOne(this, 0)
  }
  
  
  /** Returns the number of descendants the person has in total. A person's 
    * descendants are that person's children plus all their descendants. */
  def numberOfDescendants: Int = {
    generationsAlive.foldLeft(0)((n,m) => n + m.size)
  }
  
  private def generationsAlive = {
    
    def builder(start: Int, input: Vector[FamilyMember]): Vector[Vector[FamilyMember]] = {
      if(start == this.numberOfDescendingGenerations) {
        Vector(Vector())
      } else {
        val nextUp = Vector(input.map(_.children).flatten)
        nextUp ++ builder(start + 1, nextUp.flatten)
      }
    }
    builder(0, Vector(this))
  }
  
  /** Returns the number of descendants the `FamilyMember` has in a particular generation  
    * (not above, not below). A generation is defined here as a number of "steps" that indicates  
    * how far removed a set of descendants is from their ancestor. For example, a person's  
    * children form a generation at step 1, the person's grandchildren constitute another 
    * generation at step 2, the greatgrandchildren at step 3, and so on. 
    * @param generation  a positive number of "steps" that identifies a generation
    * @return the number of descendants that are ''exactly'' the indicated number of steps 
    *         removed from the person in the family tree */
   def numberOfDescendantsAt(generation: Int): Int = {
    if(generation <= 0) 0 
    else generationsAlive(generation - 1).size
    }

  
  /** Returns the person in the family tree from this `FamilyMember` downwards
    * (this person included) that has the largest number of children. Grandchildren 
    * are not included. If called on a childless person, returns that person.
    * In case of a tie, the method will return one of the tied people.
    * 
    * Consider an example family tree:
    * 
    *  - Odin has one child: Edie.
    *  - Edie has five children: Molly, Barbara, Calvin, Sam, and Walter.
    *  - Sam has three children: Dawn, Gus, and Greg. 
    *  - Dawn has three children: Lewis, Milton, and Edith.
    * 
    * This means that: 
    * 
    *  - Called on Odin, the method returns Edie, since she has five children;
    *    more than her parent Odin and more than any of her own descendants.
    *  - Called on Edie, the method returns Edie herself, for the same reason.
    *  - Called on Molly, the method returns Molly herself, even though she has 
    *    no children. 
    *  - Called on Sam, the method may return either Sam himself or Sam's daughter 
    *    Dawn, both of whom have three children. */
  def mostChildren: FamilyMember = {
    
    def childComparison(parent1: FamilyMember, parent2: FamilyMember) = {if(parent1.children.size >= parent2.children.size) parent1 else parent2}
    
    def checkMultiple(parent: FamilyMember, start: Int): FamilyMember = {
      if(parent.children.size == start + 1){
        parent.children(start)
      } else {
        val bestOfOthers = checkMultiple(parent, start + 1)
        childComparison(childCheck(parent.children(start)), childCheck(bestOfOthers))
      }
    }
    def childCheck(input: FamilyMember): FamilyMember = {
      if(input.children.size == 0) {
        input
      } else {
        val bestKid = checkMultiple(input, 0)
        childComparison(input, bestKid)
      }
    }
    childCheck(this)
  }

  
  /** Returns a string representation of the person. */
  override def toString = if (this.isChildless) this.name else (this.name + "(" + this.children.mkString(",") + ")").take(500) 
  
  
}