package pso.halt

import pso.stats.Stats

/** Halt according to fitness evaluations */
class HaltConditionEvaluations(val maxEvaluations: Int) extends HaltCondition {
  require(maxEvaluations > 0, "maxEvaluations must be positive!")
  
  /**
   * Halt condition for evaluations
   * @param state The stats for the current run
   * @return Whether or not we halt
   */
  override def shouldHalt(state: Stats): Boolean = {
    state.fitnessEvaluations > maxEvaluations
  }
}