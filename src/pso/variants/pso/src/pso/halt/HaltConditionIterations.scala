package pso.halt

import pso.stats.Stats

class HaltConditionIterations(val maxIterations: Int) extends HaltCondition {
  require(maxIterations > 0, "maxIterations must be positive!")
  
  override def shouldHalt(state: Stats): Boolean = {
    state.iterations > maxIterations
  }
}