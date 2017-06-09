package pso.variants.sopso
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

/**
 * A class to represent a PSO with particles that self-organize
 * 
 * @constructor Creates a PSO with particles that self-organize
 * @param parameters A set of various parameters
 */
class PSOSelfOrg (parameters : Parameters, vMax : Double) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new ParticleSelfOrg(parameters, vMax))
  var globalBestParticle = getBestParticle()
  
  /**
   * Updates the self-organizing particle population 
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