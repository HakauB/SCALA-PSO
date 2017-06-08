package pso.variants.tvapso
import pso.core.Parameters
import pso.core.PSO
import pso.core.Particle
import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

class PSOTimeVaryAcc (parameters : Parameters, var weightPB : Double, var weightGB : Double, maxIt : Int) extends PSO(parameters) {
  var particleArray: ParArray[Particle] = ParArray.fill[Particle](parameters.popSize)(new ParticleTimeVaryAcc(parameters, weightPB, weightGB, maxIt))
  var globalBestParticle = getBestParticle()
  
  var iterations : Int = 0
  
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