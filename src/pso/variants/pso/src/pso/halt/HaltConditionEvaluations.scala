package pso.halt

import pso.stats.Stats

class HaltConditionEvaluations(val maxEvaluations: Int) extends HaltCondition {
  require(maxEvaluations > 0, "maxEvaluations must be positive!")
  
  override def shouldHalt(state: Stats): Boolean = {
    state.fitnessEvaluations > maxEvaluations
  }
}