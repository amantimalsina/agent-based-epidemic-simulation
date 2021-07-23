package Simulation.Objects


class Agent(var group: Group) {
  var viralLoad: Double = 0.0
  var infectedSince: Int = 0
  var category: Int = 0
  var tempCategory = 0
  var sigmoidPoint: Int = 0
  var rateOfInflexion: Double = 0
  var immunityConstant: Double = 0.0
  var threshold: Map[Int, Double] = Map()
  var vaccination_status: Boolean = false

  def infected(): Boolean = {
    if (viralLoad > 0.2) true
    else false
  }



  def updateInfectionRecovery(upCategory: Boolean, downCategory: Boolean, tempCategory: Int): Unit = {
    if (infected()) {
      infectedSince += 1
    }
    if (!vaccination_status) {
      // Viral Load Update
      if (upCategory) viralLoad = threshold(tempCategory) + 0.001
      else if (downCategory) viralLoad = threshold(this.category+1) - 0.01
      else if (category > 0) viralLoad = viralLoad / (1 + math.pow(math.exp(1), rateOfInflexion * (infectedSince - sigmoidPoint)))

      // Category Update
      if (this.category > 1 && viralLoad < threshold(this.category - 1)) {
        this.category -= 1
        if (this.category < 2) {
          this.group.nonHospitalizedMembers.append(this)
        }
      }

      if (this.category == 1 && viralLoad <= 0.2) {
        infectedSince = 0
        this.category = 0
        this.group.infectedMemberSize = Math.max(0, this.group.infectedMemberSize - 1)
      }
    }
    else{
      this.viralLoad /= 2
    }
  }
}
