package pso.halt

import pso.stats.Stats

/** Halt condition according to time */
class HaltConditionTime(val maxRunTime: Long) extends HaltCondition {
  require(maxRunTime > 0, "maxRunTime must be positive!")
  
  /**
   * Halt condition met or not according to time
   * @param state The stats for the current run
   * @return Whether or not we halt
   */
  override def shouldHalt(state: Stats): Boolean = {
    state.getCurrentRunTime() > maxRunTime
  }
}