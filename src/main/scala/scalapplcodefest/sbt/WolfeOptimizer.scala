package scalapplcodefest.sbt

import scala.tools.nsc.Settings
import scalapplcodefest.compiler
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{VirtualDirectory, AbstractFile}
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scalapplcodefest.newExamples.CoinTossing
import cc.factorie.WeightsSet
import scalapplcodefest.compiler.CompilerHelpers

/**
 * @author Sebastian Riedel
 */
object WolfeOptimizer {

  /**
   * This method takes an object, searches for the source code of the object's class, generates
   * optimized code for this class, and then instantiates an object of the optimized class.
   * It hence returns an optimized version of the uncompiled input.
   */
  def optimizeClass[T](uncompiled: Class[_], replacers: List[GeneratorEnvironment => CodeStringReplacer]): Class[T] = {
    //first generate optimized/replaced source code
    val packageName = uncompiled.getPackage.getName
    val className = uncompiled.getSimpleName
    val sourceDir = "src/main/scala/"
    val managedSourceDir = "target/scala-2.10/sbt-0.13/src_managed/main/scala/"
    val sourceFileName = uncompiled.getName.replaceAll("\\.", "/") + ".scala"
    val sourceFile = new java.io.File(sourceDir + sourceFileName)
    GenerateSources.generate(sourceFile.getAbsolutePath, managedSourceDir, replacers)

    //now compile and run the generated code
    val outputDir = new VirtualDirectory("(memory)", None)
    val compiledSourceFile = new java.io.File(s"$managedSourceDir${packageName.replaceAll("\\.", "/")}/${className}Compiled.scala")
    val compiledClassName = s"$packageName.${className}Compiled"
    val source = new BatchSourceFile(AbstractFile.getFile(compiledSourceFile))

    //compiler settings
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.classpath.append(compiler.dirPathOfClass(getClass.getName))
    println(CompilerHelpers.jarPathOfClass("cc.factorie.WeightsSet"))
//    CompilerHelpers.jarPathOfClass("cc.factorie.WeightsSet").foreach(settings.classpath.append(_))
    CompilerHelpers.jarPathOfClass("cc.factorie.WeightsSet").foreach(settings.bootclasspath.append(_))
    CompilerHelpers.jarPathOfClass("gnu.trove.map.custom_hash.TObjectIntCustomHashMap").foreach(settings.bootclasspath.append(_))
    settings.bootclasspath.append(compiler.dirPathOfClass(getClass.getName))
    settings.outputDirs.setSingleOutput(outputDir)

    SimpleCompiler.compile(settings, List(source), Nil)

    //creating an instance of the generated class
    val classLoader = new AbstractFileClassLoader(outputDir, this.getClass.getClassLoader)
//    val classLoader = new AbstractFileClassLoader(outputDir, ArgminByFactorieTrainer.getClass.getClassLoader)
    val cls = classLoader.loadClass(compiledClassName)
    cls.asInstanceOf[Class[T]]
  }


  /**
   * This method takes an object, searches for the source code of the object's class, generates
   * optimized code for this class, and then instantiates an object of the optimized class.
   * It hence returns an optimized version of the uncompiled input.
   */
  def optimizeObject[T](uncompiled: Any, replacers: List[GeneratorEnvironment => CodeStringReplacer]): T = {
    //first generate optimized/replaced source code
    val optimizedClass = optimizeClass[T](uncompiled.getClass,replacers)
    optimizedClass.newInstance()
  }

}

object MLETest {
  def main(args: Array[String]) {
    val uncompiled = new CoinTossing
    val compiled = WolfeOptimizer.optimizeObject[() => Any](uncompiled, List(new MLECodeReplacer(_)))
    println(compiled())

  }
}
