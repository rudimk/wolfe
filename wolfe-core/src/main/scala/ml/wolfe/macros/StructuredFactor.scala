package ml.wolfe.macros

import ml.wolfe.FactorGraph

/**
 * Represents a factor that decomposes into sub-factors, and provides a potential over
 * values of type `T`.
 * @author Sebastian Riedel
 */
trait StructuredFactor[T] {

  /**
   * @return the top level structure corresponding to all nodes of the factor graph.
   */
  def structure: Structure[T]

  /**
   * @return the sub-structures involved in this factor.
   */
  def arguments: List[Structure[_]]

  /**
   * @return the factor graph factors this structured factor corresponds to.
   */
  def factors: Iterator[FactorGraph.Factor]

  /**
   * @return the score of the structures current setting under the factor's potential.
   */
  def potential(): Double = {
    factors.map(_.potential.valueForCurrentSetting()).sum
  }

  /**
   * @return the underlying factor graph.
   */
  def graph = structure.graph
}

