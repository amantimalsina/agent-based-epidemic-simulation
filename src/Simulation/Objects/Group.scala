package Simulation.Objects

import scala.collection.mutable
import scala.util.Random

class Group(size: Int) {
  var totalVisits: Int = 0
  var familyMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  var infectedMemberSize: Int = 0


  def initialize(): Unit = {
    for (_ <- 0 until size) {
      val newMember: Agent = new Agent()
      familyMembers.append(newMember)
    }
  }


  def visitPublicPlace(publicPlace: PublicPlace): Unit = {
    val agentToVisit: Agent = familyMembers(Random.nextInt(familyMembers.size)) /** Choose a random member. */
    agentToVisit.viralLoad += (publicPlace.aggregateViralLoad/publicPlace.capacity)
    publicPlace.incrementViralLoad(agentToVisit)
  }

  def updateCategory(transitionProbabilities: Map[(Int, Int), Double], viralLoadThreshold: Map[Int, Double]): Unit = {
    val initBuffer1 = mutable.ArrayBuffer[Agent]()
    val initBuffer2 = mutable.ArrayBuffer[Agent]()
    val initBuffer3 = mutable.ArrayBuffer[Agent]()
    val initBuffer4 = mutable.ArrayBuffer[Agent]()
    val currentSituation: mutable.Map[Int, mutable.ArrayBuffer[Agent]] = mutable.Map(1 -> initBuffer1, 2 -> initBuffer2, 3 -> initBuffer3, 4 -> initBuffer4)
    /** Determining how many and which agents have crossed the viral load threshold
     *  and putting them into the appropriate buffers. */
    for (agent <- familyMembers) {
      val category = agent.category
      val limit = viralLoadThreshold(category+1)
      if (agent.viralLoad >= limit) {
        val bufferCategory = currentSituation(category+1)
        bufferCategory.append(agent)
        currentSituation(category+1) = bufferCategory
      }
      if (category != 0) agent.updateInfectionRecovery()
    }
    /** Now, we need to use the transition probabilities to determine how many members' categories is going to be changed. */
    for ((categoryNum, buffer) <- currentSituation) {
      if (categoryNum == 1) {
        for (agent <- buffer) {
          agent.category += 1
          infectedMemberSize += 1
        }
      }
      else {
        val tP = transitionProbabilities((categoryNum - 1, categoryNum))
        val bufferSize = buffer.size
        val transitioningNumber = (bufferSize * tP).toInt
        for (_ <- 0 until transitioningNumber) {
          val agent = buffer(Random.nextInt(bufferSize))
          agent.category += 1
          if (agent.category == 4) {
            familyMembers.subtractOne(agent) // Agent deceased!
          }
        }
      }
    }
  }
}
