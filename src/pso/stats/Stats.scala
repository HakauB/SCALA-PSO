package pso.stats

import scala.collection.parallel.mutable.ParArray
import pso.core.Parameters
import pso.core.Particle

/**
 * Represents a record of information from a PSO run.
 * 
 * It is used by PSO variants to check the halt condition and is minimally maintained by the base PSO class to achieve this
 */
class Stats() {
  
  /** tracking variables for potential halt conditions */
  var iterations: Int = 0
  var fitnessEvaluations: Int = 0
  val startTime: Long = System.currentTimeMillis()  // Defer to java call
  var finishTime: Long = -1L
  
  var fitnessMean: Double = 0.0
  
  def incIterations(): Unit = {
    iterations+=1
  }
  
  /**
   * Marks the PSO run as finished
   * 
   * To be called only after the mainLoop of a PSO object is finished. Finish should only be called once per PSO
   * 
   * @return The total run-time of the algorithm
   */
  def finished() : Long = {
    //if we haven't set the finish time
    if (finishTime == -1L) {
      //set it
      finishTime = System.currentTimeMillis()
    }
    //return total runtime
    finishTime - startTime
  }
  
  /**
   * Gets the current run-time of the PSO 
   * 
   * @return the total run-time of the PSO, provided finished() has been called. Returns the current run-time otherwise
   */
  def getCurrentRunTime() : Long = {
    if (finishTime == -1L) {
      System.currentTimeMillis() - startTime
    } else {
      finishTime - startTime
    }
  }
  
  /**
   * Maintains a count of the number of fitness evaluations
   * 
   * @param evals the number to increase fitnessEvaluations by
   */
  def incFitnessEvaluations(evals: Int): Unit =  {
    fitnessEvaluations+=evals
  }
  
  /**
   * Updates the mean fitness of the run with a single fitness
   * 
   * @param fitness The new fitness
   */
  def incFitnessMean(fitness: Double):Unit = {
    fitnessMean = (fitnessMean * fitnessEvaluations + fitness) / (fitnessEvaluations + 1)
  }
  
  /**
   * Updates the mean fitness of the run with fitnesses
   * 
   * @param fitnesses an array of new fitnesses
   */
  def incFitnessMean(fitnesses: ParArray[Double]): Unit = {
    val fitnessSum = fitnesses.reduceLeft(_ + _)
    fitnessMean = (fitnessMean * fitnessEvaluations + fitnessSum) / (fitnessEvaluations + fitnesses.length)
  }
  
  /**
   * Calculates the fitness variance from a group of particles
   * 
   * @param particles the group of particles
   * @return the variance of the group
   */
  def calculateVariance(particles: ParArray[Particle]): Double = {
    val fitnesses: ParArray[Double] = particles.par.map({
      case p => p.currentFitness})
    val fitnessSum = fitnesses.par.reduceLeft(_+_)
    val currentMean = fitnessSum / particles.length
    val diffs: ParArray[Double] = fitnesses.par.map({
      case f => f - currentMean})
    val diffsSquared = diffs.par.map({
      case d => d * d})
    diffsSquared.par.reduceLeft(_+_) / diffsSquared.length
  }
  
  /**
   * Applies updates to this that occur during normal operation
   * 
   * @param newFitness the fitness of a new particle
   * @param parameters the parameters given to the PSO
   */
  def updateStat(newFitness: Double, parameters : Parameters): Unit = {
    incFitnessMean(newFitness)
    incFitnessEvaluations(1)
    incIterations()
  }
  
  /**
   * Applies updates to this that occur during normal operation
   * 
   * @param particles A group of new particles
   * @param parameters the parameters given to the PSO
   */
  def updateStats(particles: ParArray[Particle], parameters: Parameters): Unit = {
    val fitnesses = particles.par.map({
      case p => p.currentFitness})
    incFitnessMean(fitnesses)
    incFitnessEvaluations(parameters.popSize)
    incIterations()
  }
}