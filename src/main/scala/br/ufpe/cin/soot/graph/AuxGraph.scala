package br.ufpe.cin.soot.graph

sealed trait EdgeType

case object SimpleEdge extends EdgeType { def instance: SimpleEdge.type = this }
case object TrueEdge extends EdgeType { def instance: TrueEdge.type = this }
case object FalseEdge extends EdgeType { def instance: FalseEdge.type = this }
case object LoopEdge extends EdgeType { def instance: LoopEdge.type = this }
case object DefEdge extends EdgeType { def instance: DefEdge.type = this }

trait LambdaLabel {
  type T
  var value: T
  val edgeType: EdgeType
}


object EdgeType {
  def convert(edge: String): EdgeType = {
    if(edge.equals(TrueEdge.toString)) {
      TrueEdge
    } else if (edge.equals(FalseEdge.toString)) {
      FalseEdge
    }else if (edge.equals(LoopEdge.toString)){
      LoopEdge
    }else if (edge.equals(DefEdge.toString)){
      DefEdge
    }
    else SimpleEdge
  }
}

sealed trait PDGType extends LabelType

case object LoopLabel extends PDGType { def instance: LoopLabel.type = this }
case object TrueLabel extends PDGType { def instance: TrueLabel.type = this }
case object FalseLabel extends PDGType { def instance: FalseLabel.type = this }
case object DefLabel extends PDGType { def instance: DefLabel.type = this }


case class TrueLabelType(labelT: PDGType) extends EdgeLabel {
  override type T = PDGType
  override var value = labelT
  override val labelType: LabelType = TrueLabel
}

case class FalseLabelType(labelT: PDGType) extends EdgeLabel {
  override type T = PDGType
  override var value = labelT
  override val labelType: LabelType = FalseLabel
}

case class DefLabelType(labelT: PDGType) extends EdgeLabel {
  override type T = PDGType
  override var value = labelT
  override val labelType: LabelType = DefLabel
}

class AuxGraph extends Graph{

  override def addEdge(source: GraphNode, target: GraphNode, label: EdgeLabel): Unit = {
    implicit val factory = scalax.collection.edge.LkDiEdge
    graph.addLEdge(source, target)(label)
  }
}


//case class StmtDef(className: String, method: String, stmt: String, line: Int)
//
//case class StatementCD(className: String, method: String, stmt: String, line: Int, sootUnit: soot.Unit = null, sootMethod: soot.SootMethod = null)
//
//case class StatementNodeCD(stmt: StmtDef, stmtType: NodeType) extends GraphNode {
//  override type T = StmtDef
//  override val value: StmtDef = stmt
//  override val nodeType: NodeType = stmtType
//
//  override def method(): soot.SootMethod = null
//
//  override def unit(): soot.Unit = null
//
////  override def show(): String = value.stmt
//
//  override def show(): String = "(" ++ value.method + ": " + value.stmt + " - " + value.line + " <" + nodeType.toString + ">)"
//
//  override def toString: String =
//    "Node(" + value.method + "," + value.stmt + "," + value.line.toString + "," + nodeType.toString + ")"
//
//  override def equals(o: Any): Boolean = {
//    o match {
//      case stmt: StatementNode => stmt.value == value && stmt.nodeType == nodeType
//      case _ => false
//    }
//  }
//}
//
