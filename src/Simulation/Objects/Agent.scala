package Simulation.Objects

class Agent(var group: Group) {
  var viralLoad: Double = 0.0
  var infectedSince: Int = 0
  var category: Int = 0
  var sigmoidPoint: Int = 0
  var rateOfInflexion: Double = 0


  def infected(): Boolean = {
    if (viralLoad > 0.2) true
    else false
  }



  def updateInfectionRecovery(threshold: Map[Int, Double]): Unit = {
    if (infected()) {
      infectedSince += 1
    }

    viralLoad = viralLoad/(1+math.pow(math.exp(1),rateOfInflexion*(infectedSince - sigmoidPoint)))

    if (this.category != 0 && viralLoad < threshold(this.category)) {
      this.category -= 1
    }
    if (viralLoad <= 0.2) {
      infectedSince = 0
    }
  }
}
