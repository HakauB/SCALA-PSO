package pso.variants.tvpso

import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations

class ParticleTimeVary(parameters : Parameters, var weightPB : Double, var weightGB : Double, var maxIt : Double) extends Particle(parameters) {
    
  /** The starting particle best weight */
  val initialWeightPB : Double = weightPB
  
  /** The starting global best weight */
  val initialWeightGB : Double = weightGB
  
  /** Updates velocity according to time variant weights
   * 
   * @param globalBest The global best solution's particle
   * @return The updated velocity
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    weightPB = (initialWeightPB - weightPB) * (1.0 / maxIt) + weightPB
    weightGB = (initialWeightGB - weightGB) * (1.0 / maxIt) + weightGB
    
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 weightPB * PSOUtils.randomDouble() * (bp - x) +
                                 weightGB * PSOUtils.randomDouble() * (bg - x)})
     
  }
}