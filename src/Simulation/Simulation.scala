package Simulation

import Simulation.Objects.{Agent, Group, PublicPlace}

import scala.collection.mutable
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}
import scala.util.control._

/** The actual parameters should be: transition probability and public place capacity. */
class Simulation(var totalPopulation: Int,  indoorInfectionRate: Double,
                 totalTimeStep: Int, numberOfPublicPlaces: Int,
                 publicPlaceOccupancyRate: Double, epsilonCategory: Map[String, Double],
                 seedPopulationPercentage: Double, perDayVaccinationRate: Double, initialViralLoad: Double,
                 transitionProbabilities: Map[(Int, Int), Double], viralLoadThreshold: Map[Int, Double]) {

  val allMembers: mutable.ArrayBuffer[Agent] = new mutable.ArrayBuffer[Agent]()
  val allGroups: mutable.ArrayBuffer[Group] = new mutable.ArrayBuffer[Group]()
  val allPublicPlaces: mutable.ArrayBuffer[PublicPlace] = new mutable.ArrayBuffer[PublicPlace]()
  var numberOfDeaths: Int = 0
  var numberOfVaccinatedPopulation: Int = 0
  var publicPlaceConstant: Double = 0

  /** Group Categorization */
  var zerothVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var firstVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var secondVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var thirdVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var fourthVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var visitPool: mutable.Map[Int, mutable.Set[Group]] = mutable.Map(0 -> zerothVisitCompleted, 1 -> firstVisitCompleted, 2 -> secondVisitCompleted, 3 -> thirdVisitCompleted, 4 -> fourthVisitCompleted)

  def initializeGroups(): Unit = {
    var populationTracker: Int = 0
    while (populationTracker < totalPopulation) {
      val randomSize = Random.between(1, 6)
      val group: Group = new Group(randomSize)
      allGroups.append(group)
      zerothVisitCompleted.addOne(group)
      /* Initializing agents in each group */
      for (_ <- 0 until randomSize) {
        val member: Agent = new Agent(group)
        member.sigmoidPoint = Random.between(10, 50)
        member.rateOfInflexion = Random.between(0.9, 1.1)
        member.immunityConstant = 1.0 //Random.between(0.0, 1.0)
        member.threshold = viralLoadThreshold
        group.familyMembers.append(member)
        group.nonHospitalizedMembers.append(member)
        allMembers.append(member)
      }
      populationTracker += randomSize
    }
    totalPopulation = populationTracker
  }

  def initializeSeedPopulation(): Unit = {
    val seedPopulation: Int = (totalPopulation * seedPopulationPercentage).toInt
    for (_ <- 0 to seedPopulation) {
      while (!infectMember()){
        //
      }
    }
  }

  def infectMember(): Boolean = {
    /** Choose a random group. */
    val groupToSeed: Group = allGroups(Random.nextInt(allGroups.length))
    val groupMembers: mutable.ArrayBuffer[Agent] = groupToSeed.familyMembers
    /** Choose a random agent. */
    val infectMember: Agent = groupMembers(Random.between(0, groupMembers.size))
    if (infectMember.category == 0) {
      infectMember.viralLoad += initialViralLoad + 0.01
      infectMember.category += 1
      groupToSeed.infectedMemberSize += 1
      true
    }
    else{
      false
    }
  }

  def vaccinatePopulation(vaccinationRate: Double): Unit = {
    val vaccinatedPopulation: Int = (totalPopulation * vaccinationRate).toInt
    for (_ <- 0 to vaccinatedPopulation) {
        if (vaccinateMember()) numberOfVaccinatedPopulation += 1
    }
  }

  def vaccinateMember(): Boolean = {
    /** Choose a random group. */
    val groupToVaccinate: Group = allGroups(Random.nextInt(allGroups.length))
    val groupMembers: mutable.ArrayBuffer[Agent] = groupToVaccinate.familyMembers
    /** Choose a random agent. */
    val vaccinateMember: Agent = groupMembers(Random.between(0, groupMembers.size))
    if (!vaccinateMember.vaccination_status) {
      vaccinateMember.vaccination_status = true
      true
    }
    else false
  }

  def initializePublicPlace(): Unit = {
    val numLargePublicPlace: Int = numberOfPublicPlaces / 5
    val numMediumPublicPlace: Int = numberOfPublicPlaces / 3
    val numSmallPublicPlace: Int = numberOfPublicPlaces - (numLargePublicPlace + numMediumPublicPlace)
    val numMap: Map[Int, String] = Map(numLargePublicPlace -> "L", numMediumPublicPlace -> "M", numSmallPublicPlace -> "S")
    for ((number, category) <- numMap) {
      for (_ <- 0 until number) {
        val publicPlace: PublicPlace = new PublicPlace(category)
        allPublicPlaces.append(publicPlace)
      }
    }
    publicPlaceConstant = allGroups.size/numberOfPublicPlaces
  }

  def startSimulation(): mutable.Map[Int, mutable.Map[Int, Int]] = {
    val returnData: mutable.Map[Int, mutable.Map[Int, Int]] = mutable.Map()
    val initialCategoryNumbers = mutable.Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0)
    for (member <- allMembers) {
      initialCategoryNumbers(member.category) += 1
    }
    returnData(0) = initialCategoryNumbers
    // Calculating the number of single-day visits: //
    val totalVisits: Int = allGroups.size * 4
    val singleDayVisits: Int = Math.max((totalVisits.toDouble / totalTimeStep.toDouble).toInt, 1)

    for (t <- 1 to totalTimeStep) {
      val categoryNumbers: mutable.Map[Int, Int] = mutable.Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0)
      // Per day simulation: //
      for (_ <- 0 until singleDayVisits) {
        var group: Group = null
        breakable {for (poolNum <- visitPool.keys) {
          if (visitPool(poolNum).nonEmpty) {  // Ensures that the group chosen is from the lowest pool.
            group = visitPool(poolNum).head
            break
          }
        } }

        if (group != null && group.nonHospitalizedMembers.nonEmpty) {
          val currVisitNum: Int = group.totalVisits
          visitPool(currVisitNum).subtractOne(group)
          group.totalVisits += 1
          if (currVisitNum != 4) visitPool(currVisitNum + 1).addOne(group)
          val publicPlace = allPublicPlaces(Random.nextInt(allPublicPlaces.length))
          group.visitPublicPlace(publicPlace, publicPlaceOccupancyRate, epsilonCategory, publicPlaceConstant)
        }
      }

      val initBuffer1 = mutable.ArrayBuffer[Agent]()
      val initBuffer2 = mutable.ArrayBuffer[Agent]()
      val initBuffer3 = mutable.ArrayBuffer[Agent]()
      val initBuffer4 = mutable.ArrayBuffer[Agent]()
      val currentSituation: mutable.Map[Int, mutable.ArrayBuffer[Agent]] = mutable.Map(1 -> initBuffer1, 2 -> initBuffer2, 3 -> initBuffer3, 4 -> initBuffer4)

      for (group <- allGroups) {
        if (group == null || group.size == 0) {
          allGroups.subtractOne(group)
        }
        else {
          val groupMembers = group.familyMembers
          // Inserting group members to buffers. //
          for (agent <- groupMembers) {
            var category = agent.category
            val viralThreshold = agent.threshold
            var limit = viralThreshold(category+1)
            if (agent.viralLoad < limit) {
              agent.updateInfectionRecovery(upCategory = false, downCategory = false, agent.category)
            }
            else {
              while (agent.viralLoad > limit && category != 3) {
                category += 1
                limit = viralThreshold(category+1)
              }
              if (!agent.vaccination_status) {
                currentSituation(category + 1).append(agent)
                agent.tempCategory = category + 1
              }
            }
          }
        }
      }
      for ((categoryNum, buffer) <- currentSituation) {
        if (categoryNum == 1) {
          for (agent <- buffer) {
            agent.category += 1
            agent.group.infectedMemberSize += 1
          }
        }
        else {
          val tP = transitionProbabilities((categoryNum - 1, categoryNum))
          val prevBufferSize = buffer.size
          val transitioningNumber = (prevBufferSize * tP).toInt
          for (_ <- 0 until transitioningNumber) {
            val bufferSize = buffer.size
            val agent = buffer(Random.nextInt(bufferSize))
            agent.category = agent.tempCategory
            agent.group.nonHospitalizedMembers.subtractOne(agent)
            buffer.subtractOne(agent)
            // TODO: Change this update function to reflect the "jumps."
            agent.updateInfectionRecovery(upCategory = true, downCategory = false, agent.tempCategory)
            if (agent.category == 4) {
              agent.group.familyMembers.subtractOne(agent) // Agent deceased!
              agent.group.size -= 1
              allMembers.subtractOne(agent)
              numberOfDeaths += 1
              if (agent.group.size == 0) allGroups.subtractOne(agent.group)
              agent.group.infectedMemberSize -= 1
            }
          }
        }
        for (agent <- buffer) {
          agent.updateInfectionRecovery(upCategory = false, downCategory = true, agent.tempCategory)
        }
      }
      // update indoor infection (currently disabled) and time map. //
      for (member <- allMembers) {
        /* if (member.group.size != 1) {
          val infectedMembersSize = member.group.infectedMemberSize
          val increaseViralLoadBy: Double = indoorInfectionRate * (infectedMembersSize.toDouble / member.group.size.toDouble)
          member.viralLoad += ((member.viralLoad + 0.01) * increaseViralLoadBy * member.immunityConstant)
        }
        if (member.viralLoad > 0.30) {
          println("let me know!")
        }*/
        categoryNumbers(member.category) += 1
      }
      for (publicPlace <- allPublicPlaces) publicPlace.aggregateViralLoad = 0 //Setting public place's viral load to zero.
      vaccinatePopulation(perDayVaccinationRate)
      categoryNumbers(4) = numberOfDeaths
      categoryNumbers(5) = numberOfVaccinatedPopulation
      returnData += (t -> categoryNumbers)
    }
    returnData
  }

}
