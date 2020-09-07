package o1.auctionhouse


class FixedPriceSale(val description: String, val price: Int, duration: Int) {
  
  private var dayCount = 0
  private var boughtBy: Option[String] = None
  private var durationVar = duration
  
  def daysLeft: Int = durationVar - dayCount


  override def toString = this.description
  
  def buyer: Option[String] = boughtBy match {
    case Some(shopper) => Some(shopper)
    case None => None
  }
  
  def isExpired: Boolean = daysLeft <= 0
  
  def isOpen: Boolean = !isExpired && boughtBy == None
    
  def advanceOneDay(): Unit = {
    if(isOpen) dayCount += 1
  }
  
  def buy(buyer: String): Boolean = {
    if (isOpen) {
      boughtBy = Some(buyer)
      true
    } else false
  }
  
  
  

}