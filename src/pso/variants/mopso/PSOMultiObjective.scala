package pso.variants.mopso

import scala.collection.parallel.mutable.ParArray
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import pso.halt.HaltCondition


class PSOMultiObjective(parameters : Parameters, fitnessFuncs: ManyFitnessFunctions) extends PSO(parameters) {
  val paretoArchive: MultiObjectiveArchive = new MultiObjectiveArchive(fitnessFuncs)
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new Particle(parameters))
  var globalBestParticle = getBestParticle()
  
  override def initialisePopulation(): Unit = {    
    println("initial globalBest: " + globalBestParticle)
    
    particleArray.par.foreach{
      x => paretoArchive.paretoAdd(x)
    }
    
    globalBestParticle = paretoArchive.getLeader()
    
    super.initialisePopulation()
  }
  
  override def mainLoop(): Unit = {
    
    while (!parameters.haltCondition.shouldHalt(stats)) {
      
      particleArray.par.foreach{
        case p => p.update(globalBestParticle); paretoArchive.paretoAdd(p);
      }

      globalBestParticle = paretoArchive.getLeader()

      super.mainLoop()
    }
    
    println("initial globalBest: " + globalBestParticle)
    paretoArchive.printArchive
  }
}