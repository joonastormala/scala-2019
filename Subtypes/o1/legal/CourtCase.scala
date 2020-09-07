package o1.legal

class CourtCase(val plaintiff: Entity, val defendant: Entity) {
  override def toString = plaintiff.name + " v. " + defendant.name
}

