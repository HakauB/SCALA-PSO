package pso.variants.levy
import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils

/**
 * A class to represent a particle that moves according to a Levy distribution, which is a tail heavy distribution
 * 
 * @constructor Create a new particle with given Parameters
 * @param parameters A set of various parameters
 */
class ParticleLevy(parameters : Parameters) extends Particle(parameters) {
  
  /**
   * Updates the velocity according to the Levy distribution
   * 
   * @return Returns the new velocity
   * @param globalBest The particle with the global best solution
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 parameters.particleWeight * levyFlight(PSOUtils.randomDouble()) * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  /**
   * Raises a number to the power of -1/3
   * 
   * @return Returns a number to the power of -1/3
   * @param u A uniformly distributed number between 0 and 1
   */
  def levyFlight(u : Double): Double = {
	  Math.pow(u,-1.0/3.0)
  }
}