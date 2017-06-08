package pso.variants.tvpso

import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations

class ParticleTimeVary(parameters : Parameters, var weightPB : Double, var weightGB : Double, var maxIt : Double) extends Particle(parameters) {
    
  val initialWeightPB : Double = weightPB
  val initialWeightGB : Double = weightGB
  
  
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    weightPB = (initialWeightPB - weightPB) * (1.0 / maxIt) + weightPB
    weightGB = (initialWeightGB - weightGB) * (1.0 / maxIt) + weightGB
    
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 weightPB * PSOUtils.randomDouble() * (bp - x) +
                                 weightGB * PSOUtils.randomDouble() * (bg - x)})
     
  }
}