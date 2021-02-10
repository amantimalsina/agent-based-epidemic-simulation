package Simulation.Objects


class PublicPlace(var size: String) {
  var aggregateViralLoad: Double = 0

  def incrementViralLoad(agent: Agent): Unit = {
      aggregateViralLoad += agent.viralLoad
  }

  def reset(): Unit = {
    aggregateViralLoad = 0
  }
}
