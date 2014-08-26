package ml.wolfe.fg

import ml.wolfe.Wolfe.logDist._
import ml.wolfe.FactorGraph.Edge

/**
 * Created by luke on 24/08/14.
 */
object GaussianPotential {

}

final class GaussianPotential(val meanEdge:Edge, val devEdge:Edge, val xEdge:Edge) extends Potential {
  val meanVar = meanEdge.n.variable.asContinuous
  val devVar = devEdge.n.variable.asContinuous
  val xVar = xEdge.n.variable.asContinuous

  override def valueForCurrentSetting(): Double = {
    math.log(gaussian(meanVar.setting, devVar.setting)(xVar.setting))
  }

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `xEdge` => meanVar.setting = sampleGaussian(meanVar.setting, devVar.setting)
    case `meanEdge` => meanVar.setting = sampleGaussian(xVar.setting, devVar.setting)
    case `devEdge` => ???
  }
}