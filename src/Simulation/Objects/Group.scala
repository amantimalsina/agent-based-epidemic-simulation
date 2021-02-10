package Simulation.Objects

import scala.collection.mutable
import scala.util.Random

class Group(var size: Int) {
  var totalVisits: Int = 0
  var familyMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  var infectedMemberSize: Int = 0


  def visitPublicPlace(publicPlace: PublicPlace, publicPlaceOccupancyRate: Double, epsilonCategory: Map[String, Double]): Unit = {
    if (this.size < 1) {
      println("let me know!")
    }
    val agentToVisit: Agent = familyMembers(Random.nextInt(this.size)) /** Choose a random member. */
    agentToVisit.viralLoad += publicPlace.aggregateViralLoad * (publicPlaceOccupancyRate + epsilonCategory(publicPlace.size))
    publicPlace.incrementViralLoad(agentToVisit)
  }


}
