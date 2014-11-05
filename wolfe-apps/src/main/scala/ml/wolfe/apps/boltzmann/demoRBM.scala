package ml.wolfe.apps.boltzmann

import cc.factorie.la.{DenseTensor1, SparseBinaryTensor1, SingletonBinaryTensor1}
import ml.wolfe.{MoreArrayOps, BeliefPropagation, FactorGraph}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.fg.{Stats, DiscreteMsgs}

/**
 * @author Georgios Spithourakis
 */
object demoRBM extends App {
  val H = 5
  val V = 10

  val paramDim = V + H + H * V

  val zeroVector = new SparseBinaryTensor1(paramDim)


  val fg     = new FactorGraph
  val vNodes = for (i <- 0 until V) yield fg.addDiscreteNode(2, "v" + i)
  val hNodes = for (i <- 0 until H) yield fg.addDiscreteNode(2, "h" + i)

  def visibleBiasParam(i: Int) = i
  def hiddenBiasParam(j: Int) = V + j
  def pairParam(v_i: Int, h_j: Int) = V + H + H * v_i + h_j

  def toProb(array:Array[Double]) = {
    val copy = MoreArrayOps.copy(array)
    MoreArrayOps.expNormalize(copy)
    copy
  }


  val vUnary = for (i <- 0 until V) yield fg.addLinearPotential(vNodes(i), Stats(
    Array(Array(0), Array(1)),
    Array(zeroVector, new SingletonBinaryTensor1(paramDim, visibleBiasParam(i)))))

  val hUnary = for (j <- 0 until H) yield fg.addLinearPotential(hNodes(j), Stats(
    Array(Array(0), Array(1)),
    Array(zeroVector, new SingletonBinaryTensor1(paramDim, hiddenBiasParam(j)))))

  val vhBinary = for (i <- 0 until V; j <- 0 until H) yield fg.addLinearPotential(hNodes(j), vNodes(i), Stats(
    Array(Array(0, 0), Array(0, 1), Array(1, 0), Array(1, 1)),
    Array(zeroVector, zeroVector, zeroVector, new SingletonBinaryTensor1(paramDim, pairParam(i, j)))))

  //create internal structures necessary for inference
  fg.build()

  val weights = new DenseTensor1(paramDim)
  weights(visibleBiasParam(0)) = 10.0
  weights(hiddenBiasParam(0)) = -10.0
  weights(pairParam(0, 0)) = 10.0

  fg.weights = weights

  //GibbsSampling.sample(fg)
  //CD.sampleAndStartFrom(data)
  BeliefPropagation.sumProduct(10)(fg)

  for (h <- hNodes) {
    println(s"${h.variable.label}: ${toProb(h.variable.asDiscrete.b).mkString(" ")}")
  }
  for (v <- vNodes) {
    println(s"${v.variable.label}: ${toProb(v.variable.asDiscrete.b).mkString(" ")}")
  }

  println(fg.gradient)
  //println(fg.gradient)




}
