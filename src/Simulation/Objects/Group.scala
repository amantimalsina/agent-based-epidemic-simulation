package Simulation.Objects

import scala.collection.mutable
import scala.util.Random

class Group(var size: Int) {
  var totalVisits: Int = 0
  var familyMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  var infectedMemberSize: Int = 0


  def visitPublicPlace(publicPlace: PublicPlace): Unit = {
    val agentToVisit: Agent = familyMembers(Random.nextInt(size)) /** Choose a random member. */
    agentToVisit.viralLoad += (publicPlace.aggregateViralLoad*publicPlace.occupancy)/publicPlace.capacity
    publicPlace.incrementViralLoad(agentToVisit)
  }


}
