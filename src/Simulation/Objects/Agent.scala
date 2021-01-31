package Simulation.Objects

class Agent {
  var viralLoad: Double = 0.0
  var infectedSince: Int = 0
  var category: Int = 0

  def infected(): Boolean = {
    if (viralLoad > 0.0) true
    else false
  }



  def updateInfectionRecovery(): Unit = {
    infectedSince += 1
    viralLoad = (0.5 * viralLoad) max 0
  }
}
