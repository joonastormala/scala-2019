package o1.blood


class BloodType(val abo: String, val rhesus: Boolean) {


  override def toString = if (rhesus) abo + "+" else abo + "-"


  def hasSafeABOFor(recipient: BloodType) = (this.abo == "O" || this.abo == recipient.abo || ((this.abo == "A" || this.abo == "B") && (recipient.abo == "AB")))


  def hasSafeRhesusFor(recipient: BloodType) = recipient.rhesus == this.rhesus || this.rhesus == false


  def canDonateTo(recipient: BloodType) = this.hasSafeABOFor(recipient) && this.hasSafeRhesusFor(recipient)


  def canReceiveFrom(donor: BloodType) = donor.hasSafeABOFor(this) && donor.hasSafeRhesusFor(this)


}

