package pso.variants.mopso

import scala.collection.parallel.mutable.ParArray
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import pso.halt.HaltCondition


class PSOMultiObjective(parameters : Parameters) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new Particle(parameters))
  var globalBestParticle = getBestParticle()
  
  override def initialisePopulation(): Unit = {    
    println("initial globalBest: " + globalBestParticle)
    super.initialisePopulation()
  }
  
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