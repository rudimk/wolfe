package ml.wolfe.apps

import ml.wolfe.util.Iris
import ml.wolfe.util.Iris._

/**
 * @author Sebastian Riedel
 */
object CRFANN extends App {

  case class Scored(label:Iris.Label, scores:Map[Iris.Label,Double])

  def model(x:IrisFeatures)(y:Scored) = y.scores(y.label) + ann(x)(y.scores)

  def ann(x:IrisFeatures)(scores:Map[Iris.Label,Double]) = 0.0 //fromPotentials(new Ann(_))

  val dataset = loadIris()

  //train/test set split
  val (train, test) = dataset.splitAt(dataset.size / 2)

}
