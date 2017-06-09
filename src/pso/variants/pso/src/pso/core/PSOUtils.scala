package pso.core

import scala.util.Random

object PSOUtils {
  
  val random: Random = new Random()
  
  def randomRange(lower: Double, upper: Double): Double = {
    random.nextDouble()*(upper - lower) + lower
  }

  def randomDouble() : Double = {
    random.nextDouble()
  }
}