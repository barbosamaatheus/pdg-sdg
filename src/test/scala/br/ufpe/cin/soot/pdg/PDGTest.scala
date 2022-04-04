package br.ufpe.cin.soot.pdg

import br.ufpe.cin.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.ufpe.cin.soot.svfa.jimple.{FieldSenstive, Interprocedural, JSVFA}
import soot.{Scene, SootMethod}

class PDGTest extends JSVFA with Interprocedural with FieldSenstive {
  def getClassName(): String = "org.springframework.boot.context.web.SpringBootServletInitializer"
  def getMainMethod(): String = "createRootApplicationContext"

  override def sootClassPath(): String = ""

  override def applicationClassPath(): List[String] = List("/media/galileu/Arquivos/Doutorado/Pesquisa/spring-boot-1.3.0.BUILD-SNAPSHOT.jar")

  override def getEntryPoints(): List[SootMethod] = {
    val sootClass = Scene.v().getSootClass(getClassName())
    List(sootClass.getMethodByName(getMainMethod()))
  }

  override def getIncludeList(): List[String] = List(
      "java.lang.*",
      "java.util.*"
    )

  override def analyze(unit: soot.Unit): NodeType = {
    if (unit.getJavaSourceStartLineNumber == 99) {
      return SinkNode
    }
    if (unit.getJavaSourceStartLineNumber == 121 || unit.getJavaSourceStartLineNumber == 123) {
      return SourceNode
    }
    return SimpleNode
  }
}
