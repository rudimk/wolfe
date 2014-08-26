package ml.wolfe.macros

import scala.reflect.macros.Context


trait MetaAtomicStructuredFactors[C <: Context] {
  this: MetaStructuredFactors[C] =>

  import context.universe._


  def atomic(info: FactorGenerationInfo) = {
    if(info.structure.hasFiniteDomain) info.linear match {
      case true => MetaAtomicStructuredFactorLinear(info)
      case false => MetaAtomicStructuredFactorTable(info)
      //    case true => MetaAtomicStructuredFactorLinear(info.copy(potential = info.transformer(inlineFull(info.potential))))
      //    case false => MetaAtomicStructuredFactorTable(info.copy(potential = info.transformer(inlineFull(info.potential))))
    } else info.linear match {
      case true => MetaContinuousAtomicStructuredFactorLinear(info)
      case false => ???
    }
  }





  trait MetaAtomicStructuredFactor extends MetaStructuredFactor {
    val info: FactorGenerationInfo
    def perSettingArrayName: TermName
    def perSettingArrayInitializer: Tree
    def perSettingValue: Tree
    def addFactorMethod: TermName = newTermName("addFactor")
    def addEdgeMethod: TermName = newTermName("addEdge")
    def createPotential: Tree
    def children = Nil
    override def weightVector = None

    import info._

    lazy val transformedPot  = transformer(inlineFull(potential))
    lazy val className       = newTypeName(context.fresh("AtomicStructuredFactor"))
    lazy val transformedPointers = distinctByTrees(structures(transformedPot, matcher).filterNot(_.meta.observed))(_.structure)
    lazy val transformedArgs = transformedPointers.map(_.structure) //distinctTrees(structures(transformedPot, matcher).filterNot(_.meta.observed).map(_.structure))
    lazy val nodesPerArg     = transformedArgs.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(transformedPot, matcher))
    lazy val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    def inject(term: Tree) = context.resetLocalAttrs(injectStructure(term, matcher))

    lazy val perSetting = q"""
        //println(vars.map(_.setting).mkString(","))
        settings(settingIndex) = vars.map(_.setting)
        $perSettingArrayName(settingIndex) = $perSettingValue
        settingIndex += 1
      """
    lazy val loop       = transformer(loopSettingsNoDuplicates(transformedPointers) { perSetting })

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val nodes:Array[Node] = $nodes.toList.distinct.sorted.toArray
        val vars = nodes.map(_.variable.asDiscrete)
        val dims = vars.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val $perSettingArrayName = $perSettingArrayInitializer
        var settingIndex = 0
        $loop
        val factor = graph.$addFactorMethod(${MetaStructuredFactor.shortCode(context)(transformedPot)})
        val edges = nodes.view.zipWithIndex.map(p => graph.$addEdgeMethod(factor,p._1,p._2)).toArray
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$transformedArgs)
      }
    """
  }





  case class MetaAtomicStructuredFactorTable(info: FactorGenerationInfo)
  extends MetaAtomicStructuredFactor {

    import info._

    def createPotential = q"new ml.wolfe.fg.TablePotential(edges,ml.wolfe.fg.Table(settings,scores))"
    def perSettingValue = q"$injected"
    def perSettingArrayInitializer = q"Array.ofDim[Double](settingsCount)"
    def perSettingArrayName = newTermName("scores")
  }

  case class MetaAtomicStructuredFactorLinear(info: FactorGenerationInfo)
  extends MetaAtomicStructuredFactor {

    import info._


    override def addFactorMethod = if (expectations) newTermName("addExpectationFactor") else newTermName("addFactor")

    override def addEdgeMethod = if (expectations) newTermName("addExpectationEdge") else newTermName("addEdge")

    def createPotential = q"new ml.wolfe.fg.LinearPotential(edges,ml.wolfe.fg.Stats(settings,vectors),graph)"

    def perSettingValue = toOptimizedFactorieVector(injected, linearModelInfo.indexTree)
    //    def perSettingValue = inject(toOptimizedFactorieVector(potential, linearModelInfo.indexTree))
    def perSettingArrayInitializer = q"Array.ofDim[ml.wolfe.FactorieVector](settingsCount)"
    def perSettingArrayName = newTermName("vectors")
  }




  case class MetaContinuousAtomicStructuredFactorLinear(info: FactorGenerationInfo) extends MetaStructuredFactor {
    import info._

    def addFactorMethod = if (expectations) newTermName("addExpectationFactor") else newTermName("addFactor")
    def addEdgeMethod = if (expectations) newTermName("addExpectationEdge") else newTermName("addEdge")

    def createPotential: Tree = q"new ml.wolfe.fg.Potential{}"
    def children = Nil
    override def weightVector = None


    lazy val transformedPot  = transformer(inlineFull(potential))
    lazy val className       = newTypeName(context.fresh("ContinuousAtomicStructuredFactor"))
    lazy val transformedPointers = distinctByTrees(structures(transformedPot, matcher).filterNot(_.meta.observed))(_.structure)
    lazy val transformedArgs = transformedPointers.map(_.structure) //distinctTrees(structures(transformedPot, matcher).filterNot(_.meta.observed).map(_.structure))
    lazy val nodesPerArg     = transformedArgs.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(transformedPot, matcher))
    lazy val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    def inject(term: Tree) = context.resetLocalAttrs(injectStructure(term, matcher))


    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val nodes:Array[Node] = $nodes.toList.distinct.sorted.toArray
        val vars = nodes.map(_.variable)
        val factor = graph.$addFactorMethod(${MetaStructuredFactor.shortCode(context)(transformedPot)})
        val edges = nodes.view.zipWithIndex.map(p => graph.$addEdgeMethod(factor,p._1,p._2)).toArray
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$transformedArgs)
      }
    """
  }

}
