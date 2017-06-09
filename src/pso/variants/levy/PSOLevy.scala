package pso.variants.levy
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

/**
 * A class to represent a PSO with particles updated according to a Levy distribution
 * 
 * @constructor Creates a PSO with Levy updates
 * @param parameters A set of various parameters
 */
class PSOLevy (parameters : Parameters) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new ParticleLevy(parameters))
  var globalBestParticle = getBestParticle()
  
  /**
   * Updates the particle population with a Levy distribution
   */
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