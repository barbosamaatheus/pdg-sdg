package br.ufpe.cin.soot

import br.unb.cic.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.unb.cic.soot.svfa.jimple.PropagateTaint
import soot.jimple.{AssignStmt, InvokeExpr, InvokeStmt}

class DFPTestSlide2(leftchangedlines: Array[Int], rightchangedlines: Array[Int], className: String, mainMethod: String) extends JSVFATest  with PropagateTaint{
  override def getClassName(): String = className
  override def getMainMethod(): String = mainMethod

  //  override def getClassName(): String = "samples.SlideSample2"
  //  override def getMainMethod(): String = "cleanText"

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

