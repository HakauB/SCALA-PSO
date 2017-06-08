package pso.variants.tvapso
import pso.core.Particle
import pso.core.Parameters
import scala.collection.parallel.mutable.ParArray
import pso.core.PSOUtils
import pso.halt.HaltConditionIterations

class ParticleTimeVaryAcc(parameters : Parameters, var weightPB : Double, var weightGB : Double, var maxIt : Double) extends Particle(parameters) {
    
  val initialWeightPB : Double = weightPB
  val initialWeightGB : Double = weightGB
  
  var currIt : Double = 0
  
  override def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    weightPB = (initialWeightGB - weightPB * (currIt / maxIt) + weightPB)
    weightGB = (initialWeightPB - weightGB * (currIt / maxIt) + weightGB)
    
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 weightPB * PSOUtils.randomDouble() * (bp - x) +
                                 weightGB * PSOUtils.randomDouble() * (bg - x)})
     
  }
  
  def updateCurrIt(newCurrIt : Double): Unit = {
    currIt = newCurrIt
  }
}