package br.ufpe.cin.soot

import br.unb.cic.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}
import br.unb.cic.soot.svfa.{CG, CHA, SPARK}
import soot.SootMethod

class DFPTest(leftchangedlines: Array[Int], rightchangedlines: Array[Int], className: String, mainMethod: String) extends JDFPTest {
  override def getClassName(): String = className
  override def getMainMethod(): String = mainMethod

  override  def callGraph(): CG = SPARK



//  override def getClassName(): String = "samples.BlackBoard"
//  override def getMainMethod(): String = "main"

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

