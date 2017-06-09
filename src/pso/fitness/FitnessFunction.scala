package pso.fitness

import scala.collection.parallel.mutable.ParArray

/** Defines a fitness function */
trait FitnessFunction {
  
  /**
   * @return The fitness
   */
  def getFitness(solution : ParArray[Double]) : Double
    
}