package pso.core

import scala.collection.parallel.mutable.ParArray
import pso.stats.Stats

abstract class PSO(val parameters : Parameters) {
  require(parameters != null, "Parameters cannot be null")
  
  var globalBestParticle : Particle
  var particleArray : ParArray[Particle]
  var stats : Stats = new Stats()
  
  def initialisePopulation(): Unit = {
   
   stats.updateStats(particleArray, parameters)
   println("initial Fitness mean: " + stats.fitnessMean)
  }
  
  def mainLoop(): Unit = {
    stats.updateStats(particleArray, parameters)
  }
  
  def getBestParticle(): Particle = {
    def max(x: Particle, y: Particle): Particle = {
      if (x.bestFitness > y.bestFitness) x else y
    }
    particleArray.par.reduceLeft(max)
  }
  
  def runPSO(): Unit = {
    initialisePopulation()
    mainLoop()
  }
}