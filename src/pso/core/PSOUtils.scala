package pso.core

import scala.util.Random

/** Holds utility methods 
 *  
 */
object PSOUtils {
  
  /** Random value */
  val random: Random = new Random()
  
  /** Gets the next random double in range
   *  
   *  @param lower The lower bound
   *  @param upper The upper bound
   *  @return The random value
   */
  def randomRange(lower: Double, upper: Double): Double = {
    random.nextDouble()*(upper - lower) + lower
  }

  /** Gets the next random
   *  
   *  @return The random value
   */
  def randomDouble() : Double = {
    random.nextDouble()
  }
  
  /** Gets the next velocity for particle
   *  
   *  @return The random value
   */
  def randomInt(max: Int): Int = {
    random.nextInt(max)
  }
  
  /** Gets the next gaussian
   *  
   *  @param lower The lower bound
   *  @param upper The upper bound
   *  @return The random value
   */
  def randomGaussRange(lower: Double, upper: Double): Double = {
    random.nextGaussian*(upper - lower) + lower
  }
  
  /** Gets the next gaussian
   *  
   *  @param lower The lower bound
   *  @param upper The upper bound
   *  @return The random value
   */
  def randomGauss(): Double = {
    random.nextGaussian
  }
}