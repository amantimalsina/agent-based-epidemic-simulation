package Simulation

import Simulation.Objects.{Agent, Group, PublicPlace}

import scala.collection.mutable
import scala.util.Random

/** The actual parameters should be: transition probability and public place capacity. */
class Simulation(var totalPopulation: Int,  indoorInfectionRate: Double,
                 totalTimeStep: Int, numberOfPublicPlaces: Int, publicPlaceOccupancyRate: Double,
                 seedPopulationPercentage: Double, initialViralLoad: Double,
                 transitionProbabilities: Map[(Int, Int), Double], viralLoadThreshold: Map[Int, Double]) {

  val allGroups: mutable.ArrayBuffer[Group] = new mutable.ArrayBuffer[Group]()
  val allPublicPlaces: mutable.ArrayBuffer[PublicPlace] = new mutable.ArrayBuffer[PublicPlace]()
  val largePublicPlaceCapacity: Int = 100
  val mediumPublicPlaceCapacity: Int = 50
  val smallPublicPlaceCapacity: Int = 25

  /** Group Categorization */
  var zerothVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var firstVisitCompleted: mutable.Set[Group] =  mutable.Set[Group]()
  var secondVisitCompleted: mutable.Set[Group] =  mutable.Set[Group]()
  var thirdVisitCompleted: mutable.Set[Group] =  mutable.Set[Group]()
  var fourthVisitCompleted: mutable.Set[Group] = mutable.Set[Group]()
  var visitPool: mutable.Map[Int, mutable.Set[Group]] = mutable.Map(0 -> zerothVisitCompleted, 1 -> firstVisitCompleted, 2 -> secondVisitCompleted, 3 -> thirdVisitCompleted, 4 -> fourthVisitCompleted)

  def initializeGroups(): Unit = {
    var populationTracker: Int = 0
    while(populationTracker < totalPopulation) {
      val randomSize = Random.between(1, 6)
      val group: Group = new Group(randomSize)
      allGroups.append(group)
      zerothVisitCompleted.addOne(group)
      /* Initializing agents in each group */
      group.initialize()
      populationTracker += randomSize
    }
    totalPopulation = populationTracker
  }

  def initializeSeedPopulation(): Unit = {
    val seedPopulation: Int = (totalPopulation * seedPopulationPercentage).toInt
    for (_ <- 0 to seedPopulation) {
      val groupToSeed: Group = allGroups(Random.nextInt(allGroups.length)) /** Choose a random group. */
      val groupMembers: mutable.ArrayBuffer[Agent] = groupToSeed.familyMembers
      val infectMember: Agent = groupMembers(Random.between(0,groupMembers.size)) /** Choose a random agent. */
      infectMember.viralLoad += initialViralLoad
      groupToSeed.updateCategory(transitionProbabilities, viralLoadThreshold)
    }
  }


  def initializePublicPlace(): Unit = {
    val numLargePublicPlace: Int = numberOfPublicPlaces/3
    val numMediumPublicPlace: Int = numberOfPublicPlaces/3
    val numSmallPublicPlace: Int = numberOfPublicPlaces - (numLargePublicPlace + numMediumPublicPlace)
    val numMap: Map[Int, Int] = Map(numLargePublicPlace -> largePublicPlaceCapacity, numMediumPublicPlace -> mediumPublicPlaceCapacity, numSmallPublicPlace -> smallPublicPlaceCapacity)
    for ((number,capacity) <- numMap) {
      for (_ <- 0 until number) {
         val publicPlace: PublicPlace = new PublicPlace(capacity)
          publicPlace.occupancy = (capacity * publicPlaceOccupancyRate).toInt
         allPublicPlaces.append(publicPlace)
      }
    }
  }

  def startSimulation(): mutable.Map[Int, mutable.Map[Int, Int]] = {
    val returnData: mutable.Map[Int, mutable.Map[Int, Int]] = mutable.Map()
    // Calculating the number of single-day visits: //
    val totalVisits: Int = allGroups.size * 4
    val singleDayVisits: Int = Math.max((totalVisits.toDouble/totalTimeStep.toDouble).toInt,1)

    for (t <- 0 until totalTimeStep) {
        val categoryNumbers: mutable.Map[Int, Int] = mutable.Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0)
        // Per day simulation: //
        for (_ <- 0 until singleDayVisits) {
          var currentPool: mutable.Set[Group] = mutable.Set()
          for (i <- 4 to 0 by -1) {
            if (visitPool(i).nonEmpty) {
              currentPool = visitPool(i) // Ensures that the group chosen is from the lowest pool.
            }
          }
          val group = currentPool.head
          val currVisitNum: Int = group.totalVisits
          visitPool(currVisitNum).subtractOne(group)
          group.totalVisits += 1
          if (currVisitNum != 4) visitPool(currVisitNum + 1).addOne(group)
          val publicPlace = allPublicPlaces(Random.nextInt(allPublicPlaces.length))
          group.visitPublicPlace(publicPlace)
          currentPool.subtractOne(group)
        }
        for (group <- allGroups) {
          // Updating indoor infection rate //
          val groupMembers = group.familyMembers
          val infectedMembersSize = group.infectedMemberSize
          val increaseViralLoadBy: Double = indoorInfectionRate * (infectedMembersSize / groupMembers.size)
          for (member <- groupMembers) {
            member.viralLoad += increaseViralLoadBy
          }
          // Update categories and viral load of members, also update the time map. //
          group.updateCategory(transitionProbabilities, viralLoadThreshold)
          for (member <- groupMembers) {
            categoryNumbers(member.category) += 1
          }
        }
        returnData += (t -> categoryNumbers)
        for (publicPlace <- allPublicPlaces) publicPlace.aggregateViralLoad = 0 //Setting public place's viral load to zero.
    }
    returnData
  }

}
