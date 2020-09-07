package o1.carsim
import o1.Pos


class Car(val fuelConsumption: Double, val tankSize: Double, private val initialFuel: Double, private val initialLocation: Pos) {
  
  private val startPoint = initialLocation
  private var currentPosition = initialLocation
  private var currentFuel = initialFuel
  private var tripDistance = 0.0
  
  
  
  def location: Pos = this.currentPosition
  
  def fuel(toBeAdded: Double): Double = {
    if (tankSize > (toBeAdded + currentFuel)) {
      currentFuel = (toBeAdded + currentFuel)
      toBeAdded
    }
    else {
      val a = tankSize - currentFuel
      currentFuel = tankSize
      a
      
    }           
  }

  def fuel(): Double = {
    fuel(tankSize)
  }

  def fuelRatio: Double = this.currentFuel / tankSize * 100.0

  def metersDriven: Double = tripDistance

  def fuelRange: Double = (tankSize * (fuelRatio/100.0)) / (fuelConsumption / 100000)
 
  def drive(destination: Pos, metersToDestination: Double): Unit = {
    
    val fuelPercent = (fuelRange / metersToDestination)
    if (fuelRange >= metersToDestination) {
      this.currentPosition = destination
      currentFuel = currentFuel - metersToDestination * (fuelConsumption / 100000)
      tripDistance = metersDriven + metersToDestination
    }
    else {
      val tripX = this.currentPosition.xDiff(destination) * fuelPercent
      val tripY = this.currentPosition.yDiff(destination) * fuelPercent      

      this.currentPosition = Pos(currentPosition.x + tripX , currentPosition.y + tripY)      
      tripDistance = metersDriven + metersToDestination * fuelPercent
      currentFuel = 0
    }
    
  }

}

