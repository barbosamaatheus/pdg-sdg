package br.ufpe.cin.soot.pdg

import br.ufpe.cin.soot.JDFPTest
import br.unb.cic.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.unb.cic.soot.svfa.jimple.{FieldSensitive, Interprocedural, JSVFA, PropagateTaint}
import soot.{Scene, SootMethod}

class PDGDataSetTest() extends JDFPTest {
  def getClassName(): String = "retrofit.RestAdapter"
  def getMainMethod(): String = "logAndReplaceRequest"

  override def sootClassPath(): String = ""

  override def applicationClassPath(): List[String] = List("/media/galileu/Arquivos/Doutorado/Pesquisa/miningframework/output/files/retrofit/2b6c719c6645f8e48dca6d0047c752069d321bc4/original-without-dependencies/merge/retrofit-1.2.3-SNAPSHOT.jar")

  override def getEntryPoints(): List[SootMethod] = {
    val sootClass = Scene.v().getSootClass(getClassName())
    List(sootClass.getMethodByName(getMainMethod()))
  }

  override def getIncludeList(): List[String] = List(
      "java.lang.*",
      "java.util.*"
    )

  override def analyze(unit: soot.Unit): NodeType = {
    if (unit.getJavaSourceStartLineNumber == 398) {
      return SinkNode
    }
    if (unit.getJavaSourceStartLineNumber == 369) {
      return SourceNode
    }
    return SimpleNode
  }
}
