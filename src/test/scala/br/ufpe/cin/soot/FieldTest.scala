package br.ufpe.cin.soot

import br.ufpe.cin.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}

class FieldTest extends JSVFATest {
  override def getClassName(): String = "samples.FieldSample"

  override def getMainMethod(): String = "main"

  override def analyze(unit: soot.Unit): NodeType = {
    if (unit.getJavaSourceStartLineNumber == 6) {
      return SourceNode
    }
    if (unit.getJavaSourceStartLineNumber == 7 || unit.getJavaSourceStartLineNumber == 11) {
      return SinkNode
    }
    return SimpleNode
  }
}
