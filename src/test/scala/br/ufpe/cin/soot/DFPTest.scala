package br.ufpe.cin.soot

import br.ufpe.cin.soot.graph._
import br.ufpe.cin.soot.analysis.jimple.PropagateTaint

class DFPTest(leftchangedlines: Array[Int], rightchangedlines: Array[Int], className: String, mainMethod: String) extends JDFPTest {
  override def getClassName(): String = className
  override def getMainMethod(): String = mainMethod

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

