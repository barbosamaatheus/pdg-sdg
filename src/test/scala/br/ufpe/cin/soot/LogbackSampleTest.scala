package br.ufpe.cin.soot

import br.ufpe.cin.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}

class LogbackSampleTest extends JSVFATest {
  override def getClassName(): String = "samples.LogbackSample"

  override def getMainMethod(): String = "main"

  // In this case, we use the source code line number
  // to state which statements are source or sink.
  override def analyze(unit: soot.Unit): NodeType =
    unit.getJavaSourceStartLineNumber match {
      case 24 => SourceNode
      case 32 | 33 => SinkNode
      case _ => SimpleNode
    }
}