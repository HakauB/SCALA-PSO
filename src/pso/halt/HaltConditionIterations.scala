package pso.halt

import pso.stats.Stats

/** Halt condition according to iterations */
class HaltConditionIterations(val maxIterations: Int) extends HaltCondition {
  require(maxIterations > 0, "maxIterations must be positive!")
  
  /**
   * Halt condition met or not according to iterations
   * @param state The stats for the current run
   * @return Whether or not we halt
   */
  override def shouldHalt(state: Stats): Boolean = {
    state.iterations > maxIterations
  }
}