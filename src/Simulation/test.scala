package Simulation

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


object test {

  def main(args: Array[String]): Unit = {
    val visitPool: mutable.Map[Int, mutable.Set[Int]] = mutable.Map(0 -> mutable.Set(1), 1 -> mutable.Set(2,3), 2 -> mutable.Set(), 3 -> mutable.Set(), 4 -> mutable.Set())
    var currentPool: mutable.Set[Int] = mutable.Set()
    for (i <- 4 to 0 by -1) {
      if (visitPool(i).nonEmpty) {
        currentPool = visitPool(i) // Ensures that the group chosen is from the lowest pool.
      }
    }
    println(currentPool)
    var group: Int = 0
    breakable { for (poolNum <- visitPool.keys) {
      if (visitPool(poolNum).nonEmpty) {
        group = visitPool(poolNum).head
        break
      }
    } }
    println(group)
    var viralLoad = 0.5
    val rateOfInflexion = 2.5
    var infectedSince = 1
    val sigmoidPoint = 13
    while (viralLoad < 0.7 ) {
      viralLoad = (0.7 - viralLoad)/(1+math.pow(math.exp(1),-1 * rateOfInflexion*(infectedSince - 1))) + viralLoad
      println(infectedSince, viralLoad)
      infectedSince += 1
      viralLoad += 0.01
    }
  }

}
