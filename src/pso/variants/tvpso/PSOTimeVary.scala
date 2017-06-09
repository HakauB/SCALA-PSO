package pso.variants.tvpso
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

/** Time-variant weight PSO */
class PSOTimeVary (parameters : Parameters, var weightPB : Double = 1.0, var weightGB : Double = 1.0, maxIt : Int) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new ParticleTimeVary(parameters, weightPB, weightGB, maxIt))
  var globalBestParticle = getBestParticle()
  
  /** Update the solutions with time-variant weights */
  override def mainLoop(): Unit = {
    while (!parameters.haltCondition.shouldHalt(stats)) {
      particleArray.par.foreach{
        case p => p.update(globalBestParticle)
      }
      val currentBestParticle = getBestParticle()
      globalBestParticle = if (currentBestParticle.bestFitness > globalBestParticle.bestFitness) currentBestParticle else globalBestParticle
      
      super.mainLoop()
    }
    println("initial globalBest: " + globalBestParticle)
  }
  
  initialisePopulation()
  mainLoop()
}