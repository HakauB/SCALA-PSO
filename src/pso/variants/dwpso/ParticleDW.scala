package pso.variants.dwpso

import pso.core.Parameters
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils

/**
 * A variant of Particle that uses a decreasing weight, supplied by a PSO
 * 
 * decreasingWeight replaces parameter.weight from the base Particle class
 */
class ParticleDW(parameters: Parameters) extends Particle(parameters) {
  /** the decreasing weight that replaces parameters.weight */
  var decreasingWeight: Double = parameters.weight
  
  /**
   * Updates velocity using decreasingWeight
   * 
   * It is otherwise the same as Particle's version of this method
   * 
   * @param globalBest the particle with the best fitness in the swarm
   * @return the new velocity vector
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => decreasingWeight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  /**
   * Sets decreasingWeight for this object
   * 
   * This should be called during every loop in the PSO's mainLoop
   * 
   * @param decreasedWeight the new value for decreasingWeight. It is up to the caller to ensure this weight is decreasing
   */
  def updateDecreasingWeight(decreasedWeight: Double):Unit = {
    decreasingWeight = decreasedWeight
  }
}