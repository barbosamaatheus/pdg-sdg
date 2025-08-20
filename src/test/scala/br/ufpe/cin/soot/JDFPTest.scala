package br.ufpe.cin.soot

import br.ufpe.cin.soot.analysis.jimple.JDFP
import br.unb.cic.soot.svfa.jimple.{FieldSensitive, Interprocedural, PropagateTaint}
import soot.{Scene, SootMethod}

import java.util
import scala.collection.JavaConverters.collectionAsScalaIterableConverter

abstract class JDFPTest extends JDFP  with Interprocedural with FieldSensitive with PropagateTaint{
  def getClassName(): String
  def getMainMethod(): String

  override def sootClassPath(): String = ""

  override def applicationClassPath(): List[String] = List("target/scala-2.12/test-classes", System.getProperty("user.home")+"/.m2/repository/javax/servlet/servlet-api/2.5/servlet-api-2.5.jar")

  override def getEntryPoints(): List[SootMethod] = {
    var mainMethods: List[SootMethod] = findMainMethods()
    if (mainMethods.isEmpty) {
      mainMethods = findPublicMethods()
    }
    mainMethods
  }

  override def getAnalysisEntryPoint(): util.List[SootMethod] = {
    val sootClass = Scene.v().getSootClass(getClassName())
    val method = sootClass.getMethodByName(getMainMethod())
    val entryPoints = new util.ArrayList[SootMethod]()
    entryPoints.add(method)
    entryPoints
  }

  override def getIncludeList(): List[String] = List(
//      "java.lang.*",
//      "java.util.*"
    )


  def findMainMethods(): List[SootMethod] = {
    val mainMethods = new util.ArrayList[SootMethod]()

    val classes = Scene.v().getApplicationClasses
    val it = classes.iterator()
    while (it.hasNext) {
      val sootClass = it.next()
      for (method <- sootClass.getMethods.asScala) {
        if (isMainMethod(method)) {
          mainMethods.add(method)
        }
      }
    }

    mainMethods.asScala.toList
  }

  private def isMainMethod(method: SootMethod): Boolean = {
    method.getName == "main" &&
      method.isStatic &&
      method.getReturnType.toString == "void" &&
      method.getParameterCount == 1 &&
      method.getParameterType(0).toString == "java.lang.String[]"
  }

  def findPublicMethods(): List[SootMethod] = {
    val publicMethods = new util.ArrayList[SootMethod]()

    val classes = Scene.v().getApplicationClasses
    val it = classes.iterator()
    while (it.hasNext) {
      val sootClass = it.next()
      for (method <- sootClass.getMethods.asScala) {
        publicMethods.add(method)
      }
    }

    publicMethods.asScala.toList
  }

}
