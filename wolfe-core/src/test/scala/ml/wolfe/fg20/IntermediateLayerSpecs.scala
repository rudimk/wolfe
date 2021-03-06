package ml.wolfe.fg20

import cc.factorie.Factorie.DenseTensor1
import cc.factorie.app.nlp.parse.ProjectiveGraphBasedParser
import ml.wolfe.{FactorieVector, Wolfe, WolfeSpec}
import org.scalatest.{FlatSpec, WordSpec}

/**
 * @author Sebastian Riedel
 */
class IntermediateLayerSpecs extends WolfeSpec {

  import Wolfe._

  "The argmax operator" should {
    "support a table potential and a case class search space" in {
      case class XY(x: String, y: String)
      val space = new ProductSearchSpace2(
        new AtomicSearchSpace[String, DiscVar[String]](new DiscVar(Seq("painter"))),
        new AtomicSearchSpace[String, DiscVar[String]](new DiscVar(Seq("noun", "verb"))),
        (x: String, y: String) => XY(x, y), (xy: XY) => xy.x, (xy: XY) => xy.y)

      //def model(xy:XY) = I(xy.x == "painter" && xy.y == "noun")

      val model = TablePotential(
        Array(space.space1.variable, space.space2.variable),
        s => I(s.disc(0) == 0 && s.disc(1) == 0))

      val result = Argmax(space)(model)
      result should be(XY("painter", "noun"))
    }
    "support a flat sum" ignore {
      def bool(name: String) = new AtomicSearchSpace.Disc[Boolean](new DiscVar(Seq(false, true), name))
      val space = new IndexedSeqSearchSpace[Boolean, AtomicSearchSpace.Disc[Boolean]](3, bool)
      val arg1 = TablePotential(Array(space.seq(0).variable, space.seq(1).variable), s => I(s.disc(0) == s.disc(1)))
      val arg2 = TablePotential(Array(space.seq(1).variable, space.seq(2).variable), s => I(s.disc(0) == s.disc(1)))
      val bias = TablePotential(Array(space.seq(0).variable), s => I(s.disc(0) == 1))
      val model = new FlatSum(Seq(arg1, arg2, bias)) {
        def argmaxer() = new MaxProduct(???)
      }
    }

    "support a perceptron loss" ignore {

      val labels = new AtomicSearchSpace.Disc[Boolean](new DiscVar(Seq(false, true)))
      val weights = new AtomicSearchSpace.Vect(new VectVar(1, "weights"))

      def feat(value: Boolean) = new DenseTensor1(Array(if (value) 1.0 else 0.0))

      def classifier[T](label: AtomicSearchSpace.Disc[T], weights: AtomicSearchSpace.Vect, feat: T => FactorieVector) =
        new LinearClassiferPotential(label, weights, feat)

      def maxPot(weights: AtomicSearchSpace.Vect) =
        new DifferentiableMaxPotential[LinearClassiferPotential[Boolean]] {
          val objective = classifier(labels, weights, feat)
          def notOptimized = weights
        }

      def observed(weights: AtomicSearchSpace.Vect)(label: Boolean) =
        new DifferentiableWithObservation() {
          val constLabel  = AtomicSearchSpace.constDisc(label)
          val observation = labels.toPartialSetting(State.single(constLabel.variable, label))
          val self        = classifier[Boolean](constLabel, weights, feat)
        }

      def negLoss(weights: AtomicSearchSpace.Vect) =
        new Sum[Differentiable] with SupportsArgmax {
          val args = Seq(
            maxPot(weights),
            ScaledPotential.scaleDifferentiable(observed(weights)(true), -1.0))
          def argmaxer() = new GradientBasedArgmaxer(this)
        }

      val toMaximize = negLoss(weights)

      val result = Argmax(weights)(toMaximize)







      //given an instance model, calculate its argmax and return statistics of winning settings for exp fam potentials
      //inner potential needs a stats(incoming,result) method
      //this is used to calculate gradients
      //then gradient-based optimizer is
      //def classifier(weights:AtomicSearchSpace.Vect, feats:AtomicSearchSpace.Vect, label:AtomicSearchSpace.Disc[T]) = ...
      //val inner = new LinearPotential(labelSpace,weightSpace) ...
      //val max = new MaxPotential(weightSpace,classifier) //needs SupportsArgmax inner potential.
      // max potentials maxes all variables of the inner potentials different to the variables of the max potential.
      //val loss = new Minus(max,inner.condition(label = gold))
      //linearPotential.gradientCalculator().gradient(observeHiddenVars) should be doable.
      //or val loss = new Minus(max, new LinearPotential(restrictedSearchSpace,weightSpace)?
      //val result = Argmax(weightsSpace)(loss)
      //trait Potential { def observed:PartialSetting = UnobservedSetting(...) }
    }



    "support a projective tree potential" ignore {
      def bool(name: String) = new AtomicSearchSpace[Boolean, DiscVar[Boolean]](new DiscVar(Seq(false, true), name))
      def double(name: String) = new AtomicSearchSpace[Double, ContVar](new ContVar(name))
      val slen = 5
      val edges = for (h <- 0 until slen; m <- 1 until slen; if h != m) yield (h, m)
      val treeSpace = new GraphSearchSpace.Disc[Boolean](0 until slen, 1 until slen, bool)
      val scoreSpace = new GraphSearchSpace.Cont(0 until slen, 1 until slen, double)
      val model = new ProjectiveTreePotential(slen, treeSpace, scoreSpace)

      //def model(xy:XY) = treePotential(slen,graph,scores)
      Argmax(treeSpace, scoreSpace.observation(Map((0, 0) -> 2.3)))(model)


    }

  }

}


class Test extends FlatSpec {



}
