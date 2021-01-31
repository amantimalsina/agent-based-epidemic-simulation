package Simulation



object main {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    /** Parameters: */
    val totalPopulation: Int = 100
    val numberOfPublicPlaces: Int = 10
    val publicPlaceOccupancyRate: Double = 0.9
    val totalTimeStep: Int = 50
    val seedPopulationPercentage: Double = 0.1
    val initialViralLoad: Double = 0.2
    val indoorInfectionRate: Double = 0.2
    /* Transition Probabilities: */
    val transitionProbabilities: Map[(Int, Int), Double] = Map((1,2)-> 0.2, (2,3) -> 0.3, (3,4) -> 0.5) // (2,3) signifies P(2,34)
    /* Viral Load Thresholds: */
    val viralLoadThreshold: Map[Int, Double] = Map(1-> 0.2, 2 -> 0.3, 3 -> 0.5, 4 -> 0.7)
    val simulation1 = new Simulation(totalPopulation, indoorInfectionRate,
      totalTimeStep, numberOfPublicPlaces, publicPlaceOccupancyRate,
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
  }
}
