package pso.variants.mopso

import pso.core.Parameters
import pso.core.Particle
import pso.core.PSOUtils

import scala.collection.parallel.mutable.ParArray

class MultiParticle(parameters: Parameters) extends Particle(parameters) {
  
  /** Gets the next velocity for particle
   *  
   *  @param globalBest The best solution particle
   *  @return The updated velocity
   */
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 parameters.particleWeight * math.abs(PSOUtils.randomGauss()) * (bp - x) +
                                 parameters.globalWeight * math.abs(PSOUtils.randomGauss()) * (bg - x)})
  }
  
  /** Clamps solutions to bounds
   *  
   *  @param position The position
   *  @param lowerBound The lower bound
   *  @param upperBound The upper bound
   *  @return The clamped position
   */
  override def clampPosition(position: Double, lowerBound: Double, upperBound:Double): Double = {
    if (position < lowerBound ) {
      if(lowerBound == 0) {
        position % (lowerBound + Double.MinValue) 
      } else {
        position % lowerBound  
      }
    } else if (position > upperBound) {
      if(upperBound == 0) {
        position % (upperBound + Double.MinValue) 
      } else {
        position % upperBound  
      }
    } else {
      position
    }
  }
}