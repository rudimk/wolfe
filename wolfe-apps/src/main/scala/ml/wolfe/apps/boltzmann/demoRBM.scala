package ml.wolfe.apps.boltzmann

import ml.wolfe.FactorGraph
import ml.wolfe.fg.DiscreteMsgs

/**
 * @author Georgios Spithourakis
 */
object demoRBM extends App{
  val H=50
  val V=100

  val fg = new FactorGraph
  val vNodes = for (i<-0 until V) yield fg.addDiscreteNode(2)
  val hNodes = for (i<-0 until H) yield fg.addDiscreteNode(2)

  //val vUnaryFactors = fg.buildFactor(Seq(vNodes(0))){_.map(_ => new DiscreteMsgs(V)))}
}
