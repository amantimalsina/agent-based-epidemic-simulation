package Simulation.Objects

import scala.collection.mutable
import scala.util.Random

class Group(var size: Int) {
  var totalVisits: Int = 0
  var familyMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  var nonHospitalizedMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  var infectedMemberSize: Int = 0


  def visitPublicPlace(publicPlace: PublicPlace, publicPlaceOccupancyRate: Double, epsilonCategory: Map[String, Double], publicPlaceConstant: Double): Unit = {
    val agentToVisit: Agent = nonHospitalizedMembers(Random.nextInt(nonHospitalizedMembers.size)) /** Choose a random member. */
    val increaseViralLoadBy = publicPlace.averageViralLoad * (publicPlaceOccupancyRate + epsilonCategory(publicPlace.size))
    agentToVisit.viralLoad += agentToVisit.immunityConstant * increaseViralLoadBy
    publicPlace.incrementViralLoad(agentToVisit)
  }

}
