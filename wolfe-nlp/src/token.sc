import ml.wolfe.{FactorGraph, FactorieVector}

trait Var[T] {

}

trait Potential {
  def contVars:Seq[Var[Double]]
  def discVars:Seq[Var[Int]]
  def valueForSettings():Double
}

trait LinearPotential extends Potential {

  def statsForSettings():FactorieVector
}


trait Edge[Msg] {

}
trait Factor[Msg] {
  def edges:IndexedSeq[Edge[Msg]]
}



trait Factor2[DiscMsg,ContMsg] {
  def contEdges:IndexedSeq[Edge[ContMsg]]
  def discEdges:IndexedSeq[Edge[DiscMsg]]

}

trait DiscOnlyFactor[Msgs] extends Factor2[Msgs,Nothing] {
  def contEdges = IndexedSeq.empty
}


trait BPMsgs {

}

trait BPPotential extends Potential {
  def marginalF2NPow(factor:Factor[BPMsgs])
}

trait FG[ContMsg,DiscMsg] {

}

object BP {

  def mkGraph(contVars:Seq[Var[Double]], potentials:Seq[BPPotential]): FG[BPMsgs,BPMsgs] = ???
  def apply(fg:FG[BPMsgs,BPMsgs]): Unit = {
    //do in-place changes to fg edges to set them to be BP messages
    //val myFactors = Factor[BPMessage]
  }
}