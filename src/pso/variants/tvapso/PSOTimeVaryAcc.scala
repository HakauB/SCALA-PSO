package pso.variants.tvapso
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

/**
 * A class to represent a PSO with particles that have parameters that vary with time
 * 
 * @constructor Creates a PSO with particles with time varying parameters
 * @param parameters A set of various parameters
 * @param weightPB The initial weighting for the personal best location in a Particle
 * @param weightGB The initial weighting for the global best location in all Particles
 * @param maxIt The maximum number of iterations these Particles can go through
 */
class PSOTimeVaryAcc (parameters : Parameters, var weightPB : Double = 1.0, var weightGB : Double = 1.0, maxIt : Int) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new ParticleTimeVaryAcc(parameters, weightPB, weightGB, maxIt))
  var globalBestParticle = getBestParticle()
  
  var iterations : Int = 0
  
  /**
   * Updates the time-varying parameters particle population 
   */
  override def mainLoop(): Unit = {
    while (!parameters.haltCondition.shouldHalt(stats)) {
      particleArray.par.foreach{
        case p => p.update(globalBestParticle); p.asInstanceOf[ParticleTimeVaryAcc].updateCurrIt(iterations)
      }
      val currentBestParticle = getBestParticle()
      globalBestParticle = if (currentBestParticle.bestFitness > globalBestParticle.bestFitness) currentBestParticle else globalBestParticle
      
      super.mainLoop()
    }
    println("initial globalBest: " + globalBestParticle)
    iterations = iterations + 1
  }
  
  initialisePopulation()
  mainLoop()
}