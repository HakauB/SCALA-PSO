package pso.variants.mopso

import scala.collection.parallel.mutable.ParArray
import pso.fitness.FitnessFunction


class MultiFitnessFunction(val functions: ParArray[FitnessFunction]) extends FitnessFunction {
  
  def getFitness(solution: ParArray[Double]): Double = {
    1
  }
  
}