package Simulation.Objects

class PublicPlace(val capacity: Int) {
  var occupancy: Int = 0
  var aggregateViralLoad: Double = 0

  def incrementViralLoad(agent: Agent): Unit = {
      aggregateViralLoad += agent.viralLoad
  }

  def reset(): Unit = {
    aggregateViralLoad = 0
  }
}
