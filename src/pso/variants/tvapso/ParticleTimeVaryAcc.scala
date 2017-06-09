package pso.variants.tvapso
import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations

/**
 * A class to represent a particle that has Time Varying parameters
 * 
 * @constructor Create a new particle with given Parameters
 * @param parameters A set of various parameters
 * @param weightPB The initial weighting for the personal best location in a Particle
 * @param weightGB The initial weighting for the global best location in all Particles
 * @param maxIt The maximum number of iterations this Particle can go through
 */
class ParticleTimeVaryAcc(parameters : Parameters, var weightPB : Double, var weightGB : Double, var maxIt : Double) extends Particle(parameters) {
    
  val initialWeightPB : Double = weightPB
  val initialWeightGB : Double = weightGB
  
  var currIt : Double = 0
  
  /**
   * Updates the velocity so that the Particle's parameters vary with time
   * 
   * @return Returns the new velocity
   * @param globalBest The particle with the global best solution
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    weightPB = (initialWeightGB - weightPB * (currIt / maxIt) + weightPB)
    weightGB = (initialWeightPB - weightGB * (currIt / maxIt) + weightGB)
    
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 weightPB * PSOUtils.randomDouble() * (bp - x) +
                                 weightGB * PSOUtils.randomDouble() * (bg - x)})
     
  }
  
  /**
   * Updates currIt so we know when we've reached maxIt iterations
   * 
   * @param newCurrIt The value to update currIt with (Should be currIt + 1)
   */
  def updateCurrIt(newCurrIt : Double): Unit = {
    currIt = newCurrIt
  }
}