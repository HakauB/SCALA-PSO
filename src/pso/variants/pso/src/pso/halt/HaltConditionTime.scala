package pso.halt

import pso.stats.Stats

class HaltConditionTime(val maxRunTime: Long) extends HaltCondition {
  require(maxRunTime > 0, "maxRunTime must be positive!")
  
  override def shouldHalt(state: Stats): Boolean = {
    state.getCurrentRunTime() > maxRunTime
  }
}