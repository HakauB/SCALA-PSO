package pso.core

import scala.collection.parallel.mutable.ParArray
import pso.core.Parameters
import pso.core.PSOUtils

class Particle(val parameters: Parameters) {
  require(parameters != null, "parameters cannot be null")
  
  var position: ParArray[Double] = generateInitialPosition()
  var velocity: ParArray[Double] = generateInitialVelocity()
  
  var bestPosition: ParArray[Double] = position
  var bestFitness: Double = parameters.fitnessFunction.getFitness(position)
  
  var currentFitness: Double = bestFitness
    
  def generateInitialPosition(): ParArray[Double] = {
    parameters.lowerBound.par.zip(parameters.upperBound).par.map({
      case (x, y) => PSOUtils.randomRange(x, y)})
  }
  
  def generateInitialVelocity(): ParArray[Double] = {
    parameters.lowerBound.par.zip(parameters.upperBound).par.map({
      case (x, y) => PSOUtils.randomRange(-math.abs(y-x), math.abs(y-x))})
  }
  
  def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  def getNextPosition(nextVelocity: ParArray[Double]): ParArray[Double] = { 
    position.par.zip(nextVelocity.par.zip(parameters.lowerBound.par.zip(parameters.upperBound))).par.map({
      case (p, (v, (l, u))) => clampPosition(p+v, l, u)})
  }
  
  def clampPosition(position: Double, lowerBound: Double, upperBound:Double): Double = {
    if (position < lowerBound) {
      lowerBound
    } else if (position > upperBound) {
      upperBound
    } else {
      position
    }
  }
  
  def update(globalBest: Particle): Double = {
    velocity = getNextVelocity(globalBest)
    position = getNextPosition(velocity)
    val newFitness = parameters.fitnessFunction.getFitness(position)
    currentFitness = newFitness
    if (newFitness > bestFitness) {
      bestFitness = newFitness
      bestPosition = position
    }
    bestFitness
  }
  
  override def toString(): String = {
    "Particle: " + position.mkString(", ") + ", fitness: " + parameters.fitnessFunction.getFitness(position)
  }
}