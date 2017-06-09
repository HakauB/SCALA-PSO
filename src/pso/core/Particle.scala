package pso.core

import scala.collection.parallel.mutable.ParArray

/**
 * Defines a particle in a PSO algorithm
 */
class Particle(val parameters: Parameters) {
  require(parameters != null, "parameters cannot be null")
  
  /** The position */
  var position: ParArray[Double] = generateInitialPosition()
  
  /** The velocity */
  var velocity: ParArray[Double] = generateInitialVelocity()
  
  /** The best position */
  var bestPosition: ParArray[Double] = position
  
  /** The best fitness */
  var bestFitness: Double = parameters.fitnessFunction.getFitness(position)
  
  /** The current fitness */
  var currentFitness: Double = bestFitness
    
  /** Generates an initial position for particle
   *  
   *  @return The starting position
   */
  def generateInitialPosition(): ParArray[Double] = {
    parameters.lowerBound.par.zip(parameters.upperBound).par.map({
      case (x, y) => PSOUtils.randomRange(x, y)})
  }
  
  /** Generates an initial velocity for particle
   *  
   *  @return The starting velocity
   */
  def generateInitialVelocity(): ParArray[Double] = {
    parameters.lowerBound.par.zip(parameters.upperBound).par.map({
      case (x, y) => PSOUtils.randomRange(-math.abs(y-x), math.abs(y-x))})
  }
  
  /** Gets the next velocity for particle
   *  
   *  @param globalBest The best solution particle
   *  @return The updated velocity
   */
  def getNextVelocity(globalBest: Particle): ParArray[Double] = {
    position.par.zip(velocity.par.zip(bestPosition.par.zip(globalBest.bestPosition))).par.map({
      case (x, (v, (bp, bg))) => parameters.weight * v +
                                 parameters.particleWeight * PSOUtils.randomDouble() * (bp - x) +
                                 parameters.globalWeight * PSOUtils.randomDouble() * (bg - x)})
  }
  
  /** Gets the next velocity for particle
   *  
   *  @param nextVelocity The velocity to update with
   *  @return The updated position
   */
  def getNextPosition(nextVelocity: ParArray[Double]): ParArray[Double] = { 
    position.par.zip(nextVelocity.par.zip(parameters.lowerBound.par.zip(parameters.upperBound))).par.map({
      case (p, (v, (l, u))) => clampPosition(p+v, l, u)})
  }
  
  /** Clamps solutions to bounds
   *  
   *  @param position The position
   *  @param lowerBound The lower bound
   *  @param upperBound The upper bound
   *  @return The clamped position
   */
  def clampPosition(position: Double, lowerBound: Double, upperBound:Double): Double = {
    if (position < lowerBound ) {
      lowerBound
    } else if (position > upperBound) {
      upperBound
    } else {
      position
    }
  }
  
  /** Updates the particle
   *  
   *  @param globalBest The global best solution
   *  @return The best particle fitness
   */
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
  
  /** Particle pretty string
   *  
   *  @return The particle's string
   */
  override def toString(): String = {
    "Particle: " + position.mkString(", ") + ", fitness: " + parameters.fitnessFunction.getFitness(position)
  }
}