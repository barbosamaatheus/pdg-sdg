package br.ufpe.cin.soot.pdg

import br.ufpe.cin.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.ufpe.cin.soot.svfa.jimple.{FieldSenstive, Interprocedural, JSVFA, PropagateTaint}
import soot.{Scene, SootMethod}

class PDGDataSetTest extends JSVFA with Interprocedural with FieldSenstive  with PropagateTaint{
  def getClassName(): String = "com.metamx.druid.loading.S3SegmentPusher"
  def getMainMethod(): String = "push(File, DataSegment)"

  override def sootClassPath(): String = ""

  override def applicationClassPath(): List[String] = List("/media/galileu/Arquivos/Doutorado/Pesquisa/druid-services-0.2.8-SNAPSHOT-jar-with-dependencies.jar")

  override def getEntryPoints(): List[SootMethod] = {
    val sootClass = Scene.v().getSootClass(getClassName())
    List(sootClass.getMethodByName(getMainMethod()))
  }

  override def getIncludeList(): List[String] = List(
      "java.lang.*",
      "java.util.*"
    )

  override def analyze(unit: soot.Unit): NodeType = {
    if (unit.getJavaSourceStartLineNumber == 66 || unit.getJavaSourceStartLineNumber == 118 ||
        unit.getJavaSourceStartLineNumber == 139 || unit.getJavaSourceStartLineNumber == 110) {
      return SinkNode
    }
    if (unit.getJavaSourceStartLineNumber == 105 || unit.getJavaSourceStartLineNumber == 125) {
      return SourceNode
    }
    return SimpleNode
  }
}
