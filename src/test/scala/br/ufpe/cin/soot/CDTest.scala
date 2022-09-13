package br.ufpe.cin.soot

import br.unb.cic.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.unb.cic.soot.svfa.jimple.PropagateTaint

class CDTest(leftchangedlines: Array[Int] , rightchangedlines: Array[Int], className: String, mainMethod: String) extends JCDTest with PropagateTaint{
  override def getClassName(): String = className
  override def getMainMethod(): String = mainMethod

  def this(){
    this(Array.empty[Int], Array.empty[Int], "", "")
  }

  override def analyze(unit: soot.Unit): NodeType = {

    if (!leftchangedlines.isEmpty && !rightchangedlines.isEmpty){
      if (leftchangedlines.contains(unit.getJavaSourceStartLineNumber)){
        return SourceNode
      } else if (rightchangedlines.contains(unit.getJavaSourceStartLineNumber)){
        return SinkNode
      }
    }

    return SimpleNode
  }

}

