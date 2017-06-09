package pso.core

import scala.collection.parallel.mutable.ParArray
import pso.fitness.FitnessFunction
import pso.halt.HaltCondition

/**
 * Defines the parameters of a PSO algorithm
 */
class Parameters(
  var fitnessFunction: FitnessFunction,
  var haltCondition: HaltCondition,
  var dimension: Int,
  var popSize: Int,
  var upperBound: ParArray[Double],
  var lowerBound: ParArray[Double],
  var weight: Double,
  var particleWeight: Double,
  var globalWeight: Double,
  ) {
  
  require(fitnessFunction != null, "Please provide a fitness function")
  require(haltCondition != null, "Please provide a halt condition")
  require(dimension > 0, "Dimension of solutions  must be greater than 0!")
  require(popSize > 0, "Population size must be greater than 0")
  require(upperBound.size == lowerBound.size, "The bound limit arrays must be of equal size!")
  require(weight >= 0.0, "Weight must be positive!")
  require(particleWeight >= 0.0, "Particle weight must be positive!")
  require(globalWeight >= 0.0, "Global weight must be positive")
  require(upperBound.par.zip(lowerBound).par.forall{case(x,y) => x > y}, "Upper bound values must be greater than lower bound values")
   
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int) = 
    this(fitnessFunction, haltCondition, dimension, 3 * dimension, ParArray.fill[Double](dimension)(1.0), ParArray.fill[Double](dimension)(0.0), 1.0, 1.0, 1.0)
  
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, upperBound: ParArray[Double], lowerBound: ParArray[Double]) = 
    this(fitnessFunction, haltCondition, upperBound.size, 3 * upperBound.size, upperBound, lowerBound, 1.0, 1.0, 1.0)
  
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int, upperBound: ParArray[Double], lowerBound: ParArray[Double]) = 
    this(fitnessFunction, haltCondition, dimension, 3 * dimension, upperBound, lowerBound, 1.0, 1.0, 1.0)
    
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int, weight: Double, particleWeight: Double, globalWeight: Double) = 
    this(fitnessFunction, haltCondition, dimension, 3 * dimension, ParArray.fill[Double](dimension)(1.0), ParArray.fill[Double](dimension)(0.0), weight, particleWeight, globalWeight)
    
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, weight: Double, particleWeight: Double, globalWeight: Double) = 
    this(fitnessFunction, haltCondition, 1, 3, ParArray.fill[Double](1)(1.0), ParArray.fill[Double](1)(0.0), weight, particleWeight, globalWeight)
    

    //handling specification of popSize
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int, popSize: Int) = 
    this(fitnessFunction, haltCondition, dimension, popSize, ParArray.fill[Double](dimension)(1.0), ParArray.fill[Double](dimension)(0.0), 1.0, 1.0, 1.0)
  
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int, popSize: Int, upperBound: ParArray[Double], lowerBound: ParArray[Double]) = 
    this(fitnessFunction, haltCondition, dimension, popSize, upperBound, lowerBound, 1.0, 1.0, 1.0)
    
  def this(fitnessFunction: FitnessFunction, haltCondition: HaltCondition, dimension: Int, popSize: Int, weight: Double, particleWeight: Double, globalWeight: Double) = 
    this(fitnessFunction, haltCondition, dimension, popSize, ParArray.fill[Double](dimension)(1.0), ParArray.fill[Double](dimension)(0.0), weight, particleWeight, globalWeight)
}