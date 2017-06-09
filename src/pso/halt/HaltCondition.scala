package pso.halt

import pso.stats.Stats

/**
 * Defines a halt condition
 */
abstract class HaltCondition {
  
  /**
   * Halt condition met or not
   * @param state The stats for the current run
   * @return Whether or not we halt
   */
  def shouldHalt(state: Stats): Boolean
}