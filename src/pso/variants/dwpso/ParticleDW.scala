package pso.variants.dwpso

import pso.core.Parameters
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils

class ParticleDW(parameters: Parameters) extends Particle(parameters) {
  var decreasingWeight: Double = parameters.weight
  
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => decreasingWeight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  def updateDecreasingWeight(decreasedWeight: Double):Unit = {
    decreasingWeight = decreasedWeight
  }
}