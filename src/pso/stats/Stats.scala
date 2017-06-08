package pso.stats

import scala.collection.parallel.mutable.ParArray
import pso.core.Parameters
import pso.core.Particle

class Stats(
    
    ) {
  
  var iterations: Int = 0
  var fitnessEvaluations: Int = 0
  
  var oldFitnessMean: Double = 0.0
  var fitnessMean: Double = 0.0
  var fitnessVariance: Double = 0.0
  
  val startTime: Long = System.currentTimeMillis()  // Defer to java call
  var finishTime: Long = -1L
  
  def incIterations(): Unit = {
    iterations+=1
  }
  
    //call this to set finishTime
  def finished() : Long = {
    //if we haven't set the finish time
    if (finishTime == -1L) {
      //set it
      finishTime = System.currentTimeMillis()
    }
    //return total runtime
    finishTime - startTime
  }
  
  //return the current runtime
  def getCurrentRunTime() : Long = {
    //if we haven't finished by calling finished()
    if (finishTime == -1L) {
      //return current run time
      System.currentTimeMillis() - startTime
    } else {
      //return current run time
      finishTime - startTime
    }
  }
  
  def incFitnessEvaluations(evals: Int): Unit =  {
    fitnessEvaluations+=evals
  }
  
  def incFitnessMean(fitness: Double):Unit = {
    fitnessMean = (fitnessMean * fitnessEvaluations + fitness) / (fitnessEvaluations + 1)
  }
  
  def incFitnessMean(fitnesses: ParArray[Double]): Unit = {
    val fitnessSum = fitnesses.reduceLeft(_ + _)
    fitnessMean = (fitnessMean * fitnessEvaluations + fitnessSum) / (fitnessEvaluations + fitnesses.length)
  }
  
  def incFitnessVariance(particles: ParArray[Particle]): Unit = {

  }
  
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
  
  
  def updateStat(newFitness: Double, parameters : Parameters): Unit = {
    incFitnessMean(newFitness)
    incFitnessEvaluations(1)
    //incFitnessVariance(newFitness)
    incIterations()
  }
  
  def updateStats(particles: ParArray[Particle], parameters: Parameters): Unit = {
    val fitnesses = particles.par.map({
      case p => p.currentFitness})
    incFitnessMean(fitnesses)
    //incFitnessVariance()
    incFitnessEvaluations(parameters.popSize)
    incIterations()
  }
}