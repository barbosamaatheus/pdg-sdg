package br.ufpe.cin.soot

import br.ufpe.cin.soot.analysis.jimple.JCD
import br.unb.cic.soot.svfa.jimple.{FieldSensitive, Interprocedural, PropagateTaint}
import soot.{Scene, SootMethod}

abstract class JCDTest extends JCD with Interprocedural with FieldSensitive with PropagateTaint {
  def getClassName(): String
  def getMainMethod(): String

  override def sootClassPath(): String = ""

  override def applicationClassPath(): List[String] = List("target/scala-2.12/test-classes", System.getProperty("user.home")+"/.m2/repository/javax/servlet/servlet-api/2.5/servlet-api-2.5.jar")

  override def getEntryPoints(): List[SootMethod] = {
    val sootClass = Scene.v().getSootClass(getClassName())
    List(sootClass.getMethodByName(getMainMethod()))
  }

  override def getIncludeList(): List[String] = List(
      "java.lang.*",
      "java.util.*"
    )
}
