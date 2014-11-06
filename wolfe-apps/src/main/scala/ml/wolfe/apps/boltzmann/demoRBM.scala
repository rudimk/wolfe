package ml.wolfe.apps.boltzmann

import cc.factorie.la.{DenseTensor1, SparseBinaryTensor1, SingletonBinaryTensor1}
import cc.factorie.optimize.{OnlineTrainer, AdaGrad}
import ml.wolfe.{GradientBasedOptimizer, MoreArrayOps, BeliefPropagation, FactorGraph}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.fg.{VectorMsgs, Potential, Stats, DiscreteMsgs}

/**
 * @author Georgios Spithourakis
 */
object demoRBM extends App {

  val H = 2
  val V = 2

  val paramDim = V + H + H * V

  val likelihoodFG = new FactorGraph

  val weights = likelihoodFG.addVectorNode(paramDim, "weights")

  val data = Seq(Seq(1, 0), Seq(0, 1))

  val factors = for (d <- data) yield {
    likelihoodFG.buildFactor(Seq(weights))(_.map(_ => new VectorMsgs)) { edges =>
      new BoltzmannLoss(edges(0), d, new RestrictedBoltzmannMachine(H, V))
    }
  }

  likelihoodFG.build()

  GradientBasedOptimizer(likelihoodFG, new OnlineTrainer(_, new AdaGrad(), 10))

  //println(fg.gradient)

}

class BoltzmannLoss(weightsEdge: Edge, observation: IndexedSeq[Int], rbm: RestrictedBoltzmannMachine) extends Potential {




  //    weights(visibleBiasParam(0)) = 10.0
  //    weights(hiddenBiasParam(0)) = -10.0
  //    weights(pairParam(0, 0)) = 10.0

  override def valueAndGradientForAllEdges() = {

    val weights = weightsEdge.msgs.asVector.n2f
    rbm.fg.weights = weights

    val gradient = new DenseTensor1(weights.size)
    var value = 0.0

    //calculate expectations with visibles fixed (positive gradient)
    for (i <- observation.indices) rbm.vNodes(i).variable.asDiscreteTyped[Int].observe(observation(i))

    //calculate expectations with visibles unobserved (negative gradient)
    BeliefPropagation.sumProduct(2)(rbm.fg)

    //add the positive gradient
    gradient += rbm.fg.gradient
    value += rbm.fg.value

    //unobserve to calculate the negative gradient
    for (n <- rbm.vNodes) n.variable.asDiscreteTyped[Int].unobserve()

    //do inference in full model without observations
    BeliefPropagation.sumProduct(10)(rbm.fg)

    //subtract the negative positive gradient
    gradient += rbm.fg.gradient
    value -= rbm.fg.value

    //pass the result to the node
    weightsEdge.msgs.asVector.f2n = gradient

    //return
    value
  }

}

class RestrictedBoltzmannMachine(val H: Int, val V: Int) {

  val paramDim = V + H + H * V

  val zeroVector = new SparseBinaryTensor1(paramDim)

  val fg     = new FactorGraph
  val vNodes = for (i <- 0 until V) yield fg.addDiscreteNode(2, "v" + i)
  val hNodes = for (i <- 0 until H) yield fg.addDiscreteNode(2, "h" + i)

  def visibleBiasParam(i: Int) = i
  def hiddenBiasParam(j: Int) = V + j
  def pairParam(v_i: Int, h_j: Int) = V + H + H * v_i + h_j

  def toProb(array: Array[Double]) = {
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


  //GibbsSampling.sample(fg)
  //CD.sampleAndStartFrom(data)
//
//  for (h <- rbm.hNodes) {
//    println(s"${ h.variable.label }: ${ rbm.toProb(h.variable.asDiscrete.b).mkString(" ") }")
//  }
//  for (v <- rbm.vNodes) {
//    println(s"${ v.variable.label }: ${ rbm.toProb(v.variable.asDiscrete.b).mkString(" ") }")
//  }
//
//  println(rbm.fg.gradient)
//
//  weightsEdge.msgs.asVector.f2n = rbm.fg.gradient

}
