package o1.family

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

  private def gens = {
    
    def helper(start: Int, input: Vector[FamilyMember]): Vector[Vector[FamilyMember]] = {
      if(start == this.numberOfDescendingGenerations) {
        Vector(Vector())
      } else {
        val nextUp = Vector(input.map(_.children).flatten)
        nextUp ++ helper(start + 1, nextUp.flatten)
      }
    }
    helper(0, Vector(this))
  }

  /** Returns the number of generations below this person in the family tree.
    * For a person with no children, this will be zero; for someone with at
    * least one child but no grandchildren, this will be one; for someone with
    * grandchildren but no greatgrandchildren, this will be two, and so on. */
  def numberOfDescendingGenerations: Int = {
    def another(input: FamilyMember, generation: Int) = {
      if (input.children.size == 0) {
        generation
      } else {
        val other = getGeneration(input, 0, generation)
        scala.math.max(generation + 1, other)
      }
    }
    def getGeneration(parent: FamilyMember, first: Int, gen: Int): Int = {
      if (parent.children.size == first + 1) {
        another(parent.children(first), (gen +1))
      } else {
        val highest = getGeneration(parent, first +1, gen)
        scala.math.max(another(parent.children(first), (gen +1)), highest)
      }
    }
    another(this, 0)
  }

  /** Returns the number of descendants the person has in total. A person's
    * descendants are that person's children plus all their descendants. */
  def numberOfDescendants: Int = {
    gens.foldLeft(0)((n,m) => n + m.size)
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
    else gens(generation - 1).size
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
    def comparison(parent1: FamilyMember, parent2: FamilyMember) = {
      if (parent1.children.size >= parent2.children.size) parent1 else parent2 }    
    def multiple(parent: FamilyMember, start: Int): FamilyMember = {
      if(parent.children.size == start + 1) { parent.children(start) } 
      else {
        val bestOfOthers = multiple(parent, start + 1)
        comparison(check(parent.children(start)), check(bestOfOthers))
      }
    }    
    def check(input: FamilyMember): FamilyMember = {
      if(input.children.size == 0) {
        input
      } else {
        val bestKid = multiple(input, 0)
        comparison(input, bestKid)
      }
    }
    check(this)
  }


  /** Returns all the people in the family tree from this `FamilyMember` downwards, including this
    * `FamilyMember`. That is, the returned collection contains this person and their descendants. */
  def everyoneBelow: Vector[FamilyMember] = {
/*    import scala.collection.mutable.Buffer
    val list = Buffer()
    list ++ Vector(this)
    list ++ this.children
    this.children.foreach(_.everyoneBelow)
    list.toVector*/
    Vector(this) ++ this.gens.flatten 
  }



  /** Returns a string representation of the person. */
  override def toString = if (this.isChildless) this.name else (this.name + "(" + this.children.mkString(",") + ")").take(500)


}