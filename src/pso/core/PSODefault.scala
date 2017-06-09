package pso.core

import scala.collection.parallel.mutable.ParArray
import pso.halt.HaltCondition

/** The standard PSO algorithm
 * 
 */
class PSODefault(parameters : Parameters) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new Particle(parameters))
  var globalBestParticle = getBestParticle()
  
  /** Initializes the algorithm population */
  override def initialisePopulation(): Unit = {
    println("initial globalBest: " + globalBestParticle)
    super.initialisePopulation()
  }
  
  /** Updates the algorithm population */
  override def mainLoop(): Unit = {
    while (!parameters.haltCondition.shouldHalt(stats)) {
      particleArray.par.foreach{
        case p => p.update(globalBestParticle)
      }
      val currentBestParticle = getBestParticle()
      globalBestParticle = if (currentBestParticle.bestFitness > globalBestParticle.bestFitness) currentBestParticle else globalBestParticle
      
      super.mainLoop()
    }
    println("final globalBest: " + globalBestParticle)
  }
}