package pso.variants.levy
import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils


class ParticleLevy(parameters : Parameters) extends Particle(parameters) {
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 parameters.particleWeight * levyFlight(PSOUtils.randomDouble()) * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  def levyFlight(u : Double): Double = {
	  Math.pow(u,-1.0/3.0)
  }
}