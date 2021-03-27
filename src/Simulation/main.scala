package Simulation


import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable.ArrayBuffer


object main {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    /** Parameters: */
    val totalPopulation: Int = 10000
    val numberOfPublicPlaces: Int = 2000
    val publicPlaceOccupancyRate: Double = 0.5
    val epsilonCategory: Map[String, Double] = Map("L" -> 0.01, "M" -> 0.03, "S" -> 0.05)
    val totalTimeStep: Int = 50
    val seedPopulationPercentage: Double = 0.05
    val initialViralLoad: Double = 0.2
    val indoorInfectionRate: Double = 0.5
    /* Transition Probabilities: */
    val transitionProbabilities: Map[(Int, Int), Double] = Map((1,2)-> 0.3, (2,3) -> 0.4, (3,4) -> 0.6) // (2,3) signifies P(2,34)
    /* Viral Load Thresholds: */
    val viralLoadThreshold: Map[Int, Double] = Map(1-> 0.2, 2 -> 0.25, 3 -> 0.35, 4 -> 0.4)
    val simulation1 = new Simulation(totalPopulation, indoorInfectionRate,
      totalTimeStep, numberOfPublicPlaces, publicPlaceOccupancyRate, epsilonCategory,
      seedPopulationPercentage, initialViralLoad, transitionProbabilities, viralLoadThreshold)

    simulation1.initializeGroups()
    println(f"# of Groups: ${simulation1.allGroups.size}")
    simulation1.initializePublicPlace()
    simulation1.initializeSeedPopulation()
    val retMap = simulation1.startSimulation()
    val endTime = System.nanoTime()
    val computingTime = endTime - startTime
    println(f"Computing Time: ${computingTime/ Math.pow(10,9)} seconds")
    println(retMap)

    // Writing to csv: //
    val outputFile = new BufferedWriter(new FileWriter(new File(s"data/pop_$totalPopulation,occ_$publicPlaceOccupancyRate.csv")))
    val allData: ArrayBuffer[ArrayBuffer[String]] = new ArrayBuffer[ArrayBuffer[String]]()
    val parameters = ArrayBuffer(s"totalPopulation: ${totalPopulation}", s"numberOfPublicPlaces: ${numberOfPublicPlaces}", s"publicPlaceOccupancyRate: ${publicPlaceOccupancyRate}", s"seedPopulationPercentage: ${seedPopulationPercentage}", s"indoorInfectionRate: ${indoorInfectionRate}")
    allData.append(parameters)
    val header = ArrayBuffer("Day #", "Category 0","Category 1","Category 2","Category 3","Category 4")
    allData.append(header)
    for ((day,map) <- retMap) {
      val tempBuffer = new ArrayBuffer[String]()
      tempBuffer.append(day.toString)
      for ((_,count) <- map) {
        tempBuffer.append(count.toString)
      }
      allData.append(tempBuffer)
    }
    for (row <- allData) {
      var tempString = new StringBuilder
      tempString = row.addString(tempString, ",")
      outputFile.write(tempString.toString() + '\n')
    }
    outputFile.close()

  }
}
