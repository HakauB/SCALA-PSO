package pso.variants.sopso
import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils
import scala.math

/**
 * A class to represent a particle that can self-organize
 * 
 * @constructor Create a new particle with given Parameters
 * @param parameters A set of various parameters
 */
class ParticleSelfOrg(parameters : Parameters, vMax : Double) extends Particle(parameters) {
  
  /**
   * Updates the velocity so that the Particle self-organizes with the other particles
   * 
   * @return Returns the new velocity
   * @param globalBest The particle with the global best solution
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => math.signum(parameters.weight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)) * 
                                 math.min(vMax, math.abs(parameters.weight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)))})
  }
}