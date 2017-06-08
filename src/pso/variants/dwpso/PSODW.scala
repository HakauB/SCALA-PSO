package pso.variants.dwpso

import pso.core.Parameters
import pso.core.Particle
import pso.variants.dwpso.ParticleDW
import pso.stats.Stats
import pso.core.PSO
import pso.halt.HaltConditionIterations
import pso.halt.HaltCondition
import scala.collection.parallel.mutable.ParArray

//http://library.ndsu.edu/tools/dspace/load/?file=/repository/bitstream/handle/10365/25404/Particle%20Swarm%20Optimization%20Algorithm%20-%20Variants%20and%20Comparisons.pdf?sequence=1
class PSODW(parameters: Parameters, initialWeight: Double = 0.9, finalWeight: Double = 0.4, maxIterations: Int = 1000) extends PSO(parameters) {
  require (initialWeight <= 1.0 && initialWeight > 0, "initialWeight must be between 0 and 1")
  require(finalWeight <= 1.0 && finalWeight > 0, "finalWeight must be between 0 and 1")
  require(initialWeight > finalWeight, "initialWeight must be more than finalWeight")
  require(maxIterations > 0, "maxIterations must be positive!")
  
  var particleArray = ParArray.fill[Particle](parameters.popSize)(new ParticleDW(parameters))
  var globalBestParticle = getBestParticle()
  
  override def mainLoop(): Unit = {
    while (!parameters.haltCondition.shouldHalt(stats)) {
      //the decreasing weight aspect -- cap the highest value for iterations/maxIterations to 1
      val iterations = if (stats.iterations > maxIterations) maxIterations else stats.iterations
      val decreasingWeight = initialWeight - (initialWeight - finalWeight) * iterations / maxIterations
      particleArray.par.foreach{
        case p => p.asInstanceOf[ParticleDW].updateDecreasingWeight(decreasingWeight); p.update(globalBestParticle)
      }
      val currentBestParticle = getBestParticle()
      globalBestParticle = if (currentBestParticle.bestFitness > globalBestParticle.bestFitness) currentBestParticle else globalBestParticle
      
      super.mainLoop()
    }
    println("final globalBest: " + globalBestParticle)
  }
}