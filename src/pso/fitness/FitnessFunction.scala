package pso.fitness

import scala.collection.parallel.mutable.ParArray

trait FitnessFunction {
  
  def getFitness(solution : ParArray[Double]) : Double
    
}