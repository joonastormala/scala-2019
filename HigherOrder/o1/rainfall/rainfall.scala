package o1

package object rainfall {

  def averageRainfall(data: Vector[Int]): Option[Int] = {
    val temp=data.filter( _ >= 0 )
    val filteredData = temp.takeWhile( _ != 999999 )
    if (filteredData.isEmpty)
      None
    else {
      Some(filteredData.foldLeft(0)(_+_)/filteredData.size)
    }
  }
  def drySpell(data:Vector[Int],lenght:Int):Int={
    for (i <- 0 until data.size){
      if(data(i)>=0 && data(i)<6){
        if(data.drop(i).take(lenght).foldLeft(true)((x,y)=>x && y>= 0 &&y<6 && y!=999999)){
            return i
        }
      }      
    }
    -1
  }





}