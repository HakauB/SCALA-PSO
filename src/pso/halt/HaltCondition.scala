package pso.halt

import pso.stats.Stats

abstract class HaltCondition {
  def shouldHalt(state: Stats): Boolean
}