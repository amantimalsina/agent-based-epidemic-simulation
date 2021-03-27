package Simulation.Objects

import scala.math.BigDecimal.double2bigDecimal

class Agent(var group: Group) {
  var viralLoad: Double = 0.0
  var infectedSince: Int = 0
  var category: Int = 0
  var sigmoidPoint: Int = 0
  var rateOfInflexion: Double = 0
  var immunityConstant: Double = 0.0

  def infected(): Boolean = {
    if (viralLoad > 0.2) true
    else false
  }



  def updateInfectionRecovery(threshold: Map[Int, Double], upCategory: Boolean, downCategory: Boolean): Unit = {
    if (infected()) {
      infectedSince += 1
    }
    // Viral Load Update
    if (upCategory) viralLoad = threshold(this.category-1) + 0.01
    else if (downCategory) viralLoad = threshold(this.category) + 0.01
    else viralLoad = viralLoad/(1+math.pow(math.exp(1),rateOfInflexion*(infectedSince - sigmoidPoint)))

    // Category Update
    if (this.category > 1 && viralLoad < threshold(this.category-1)) {
      this.category -= 1
      if (this.category < 2) {
        this.group.nonHospitalizedMembers.append(this)
      }
    }

    if (this.category == 1 && viralLoad <= 0.2) {
      infectedSince = 0
      this.category = 0
      this.group.infectedMemberSize = Math.max(0, this.group.infectedMemberSize-1)
    }
  }
}
