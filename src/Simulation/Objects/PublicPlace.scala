package Simulation.Objects


class PublicPlace(var size: String) {
  var aggregateViralLoad: Double = 0.0
  var averageViralLoad: Double = 0.0
  var totalVisitors: Int = 0

  def incrementViralLoad(agent: Agent): Unit = {
      aggregateViralLoad += agent.viralLoad
      totalVisitors += 1
      averageViralLoad = aggregateViralLoad/totalVisitors
  }

  def reset(): Unit = {
    aggregateViralLoad = 0
    averageViralLoad = 0
    totalVisitors = 0
  }
}
