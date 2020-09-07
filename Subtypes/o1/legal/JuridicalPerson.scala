package o1.legal


abstract class JuridicalPerson(name: String) extends Entity(name)

abstract class HumanOrganization(name: String, val contact: NaturalPerson) extends JuridicalPerson(name)

class GeographicalFeature(name: String, val kind: String, val representative: Entity) extends JuridicalPerson(name){
  def contact: NaturalPerson = representative.contact
}

class Group(val members: Vector[Entity]) extends JuridicalPerson(members.mkString(",")){  
  val leader: Entity = members(0)
  def contact: NaturalPerson = leader.contact
  override def kind = "group of " + members.size + " led by " + leader.contact
}





class Nation(name: String, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = "sovereign nation"

}

class Municipality(name: String, val nation: Nation, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = "municipality of " + this.nation.name
}


class Corporation(val id: String, val seeksProfit: Boolean, name: String, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = (if (this.seeksProfit) "for-" else "non-") + "profit corporation"
}


