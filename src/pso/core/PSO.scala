package pso.core

import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

abstract class PSO(val parameters : Parameters) {
  require(parameters != null, "Parameters cannot be null")
  
  var globalBestParticle : Particle
  var particleArray : ParArray[Particle]
  var stats : Stats = new Stats()
  
  /** Initializes the algorithm solutions */
  def initialisePopulation(): Unit = {
   
   stats.updateStats(particleArray, parameters)
   println("initial Fitness mean: " + stats.fitnessMean)
  }
  
  /** Updates the algorithm solutions */
  def mainLoop(): Unit = {
    stats.updateStats(particleArray, parameters)
  }
  
  /** Gets the best particle
   *  
   *  @return The best global particle
   */
  def getBestParticle(): Particle = {
    def max(x: Particle, y: Particle): Particle = {
      if (x.bestFitness > y.bestFitness) x else y
    }
    particleArray.par.reduceLeft(max)
  }
  
  /** Runs the algorithm */
  def runPSO(): ParArray[Double] = {
    initialisePopulation()
    mainLoop()
    stats.finished()
    getBestSolution()
  }
  
  /** Gets the best solution
   *  
   *  @return The best global solution
   */
  def getBestSolution(): ParArray[Double] = {
    globalBestParticle.bestPosition
  }
}