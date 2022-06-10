package br.ufpe.cin.soot.svfa.jimple

import br.ufpe.cin.soot.graph
import br.ufpe.cin.soot.graph.{ContextStatement, DefLabel, EdgeLabel, EdgeLabelType, FalseLabel, GraphNode, SinkNode, SourceNode, StatementNode, StringLabel, TrueLabel}
import scalax.collection.GraphPredef.anyToNode
import soot.jimple._
import soot.options.Options
import soot.toolkits.graph.{ExceptionalBlockGraph, ExceptionalUnitGraph}
import soot.toolkits.scalar.SimpleLocalDefs
import soot.{Local, PackManager, Scene, SceneTransformer, SootMethod, Transform}

import java.util


/**
 * A Jimple based implementation of
 * Control Dependence Analysis.
 */
abstract class JPDG extends JCD with JDFP   {

  val allocationSitesPDG = scala.collection.mutable.HashMap.empty[soot.Value, soot.Unit]
  val traversedMethodsPDG = scala.collection.mutable.Set.empty[SootMethod]
  var listDef : List[(AssignStmt, StatementNode, Int)] = List()
  var pdg = new br.ufpe.cin.soot.graph.GraphLabelKey()

  def buildPDG() {
    //    buildDFP()
    buildCD()
    buildDFP()
    mergeDFPAndCD()

    val (pack, t) = createSceneTransformPDG()
    PackManager.v().getPack(pack).add(t)
    configurePackages().foreach(p => PackManager.v().getPack(p).apply())

    afterGraphConstruction()
  }

  def mergeDFPAndCD(): Unit = {
    //Add cd edges in pdg
    for (e <- cd.edges) {
      val from = e.from
      val label = e.label
      val to = e.to
      pdg.addEdge(from, to, label)
    }

    //Add df+ edges in pdg

    for (e <- svg.edges) {
      val from = e.from
      val label = e.label
      val to = e.to
      pdg.addEdge(from, to, label)
    }


  }
  def createSceneTransformPDG(): (String, Transform) = ("wjtp", new Transform("wjtp.pdg", new TransformerPDG()))

  class TransformerPDG extends SceneTransformer {
    override def internalTransform(phaseName: String, options: util.Map[String, String]): scala.Unit = {
      pointsToAnalysis = Scene.v().getPointsToAnalysis
      initAllocationSitesPDG()
      Scene.v().getEntryPoints.forEach(method => {
        traversePDG(method)
        methods = methods + 1
      })
    }
  }

  def initAllocationSitesPDG(): scala.Unit = {
    val listener = Scene.v().getReachableMethods.listener()

    while(listener.hasNext) {
      val m = listener.next().method()
      if (m.hasActiveBody) {
        val body = m.getActiveBody
        body.getUnits.forEach(unit => {
          if (unit.isInstanceOf[soot.jimple.AssignStmt]) {
            val right = unit.asInstanceOf[soot.jimple.AssignStmt].getRightOp
            if (right.isInstanceOf[NewExpr] || right.isInstanceOf[NewArrayExpr]) {// || right.isInstanceOf[StringConstant]) {
              //            if (right.isInstanceOf[NewExpr] || right.isInstanceOf[NewArrayExpr] || right.isInstanceOf[StringConstant]) {
              allocationSitesPDG += (right -> unit)
            }
          }
        })
      }
    }
  }

  def traversePDG(method: SootMethod, forceNewTraversal: Boolean = false) : scala.Unit = {
    if((!forceNewTraversal) && (method.isPhantom || traversedMethodsPDG.contains(method))) {
      return
    }

    traversedMethodsPDG.add(method)

    val body  = method.retrieveActiveBody()
    val graph = new ExceptionalUnitGraph(body)

    val graphBlock = new ExceptionalBlockGraph(body)

    body.getUnits.forEach(unit => {
      try{
        val v = Statement.convert(unit)

        v match {
          case AssignStmt(base) => traverseDef(AssignStmt(base), unit, method, graphBlock)
          case _ =>
        }
      }catch {
        case e: Exception => return
      }
    })

    if (listDef.length>0){
      for (i <- 0 until listDef.length) {
        for (j <- i+1 until listDef.length){
          if (listDef(i)._1.stmt.getLeftOp == listDef(j)._1.stmt.getLeftOp){

            try {
              val nextI = svg.getAdjacentNodes(listDef(i)._2).get
              val nextJ = svg.getAdjacentNodes(listDef(j)._2).get

              val xx = listDef(i)._1.stmt
              val yy = listDef(j)._1.stmt
              val bx = listDef(i)._3
              val by = listDef(j)._3

              nextI.foreach(nodeI=>{
                nextJ.foreach(nodeJ=>{
                  if (nodeI.equals(nodeJ)){ //Add edge
                    addDefEdges(listDef(i)._1.stmt, listDef(j)._1.stmt, method)
                  }
                })
              })
            }catch{
              case e => print(e)
            }
          }
        }
      }
    }

  }

  def traverseDef(assignStmt: AssignStmt, unit: soot.Unit, method: SootMethod, graph: ExceptionalBlockGraph) : Unit = {
    val node = createNode(method, unit)
    var branch = -1

    graph.forEach(block => {
      block.forEach(u =>{
        if (u.equals(unit)){
          branch = block.getIndexInMethod
        }
      })
    })
    listDef = listDef:+ (assignStmt, node, branch)
  }

  def addDefEdges(s: soot.Unit, t: soot.Unit, method: SootMethod): Unit = {
    if (s.isInstanceOf[GotoStmt] || t.isInstanceOf[GotoStmt]) return
    var source = createNode(method, s)
    var target = createNode(method, t)

    val auxLabel = createDefEdgeLabel(s, t, method)

    addLoopEdge(source, target, auxLabel)
  }

  def addLoopEdge(source: GraphNode, target: GraphNode, label: EdgeLabel): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      //      val label = createLoopEdgeLabel()
      pdg.addEdge(source, target, label)
      res = true
    }
    return res
  }

  def reportConflictsPDG(): scala.collection.Set[String] =
    findConflictingPathsPDG().map(p => p.toString)

  def findConflictingPathsPDG(): scala.collection.Set[List[GraphNode]] = {
    if (pdg.fullGraph) {
      val conflicts = pdg.findPathsFullGraph()
      return conflicts.toSet
    } else {
      val sourceNodes = pdg.nodes.filter(n => n.nodeType == SourceNode)
      val sinkNodes = pdg.nodes.filter(n => n.nodeType == SinkNode)

      var conflicts: List[List[GraphNode]] = List()
      sourceNodes.foreach(source => {
        sinkNodes.foreach(sink => {
          val paths = pdg.findPath(source, sink)
          conflicts = conflicts ++ paths
        })
      })

      conflicts.filter(p => p.nonEmpty).toSet
    }
  }

  def pdgToDotModel(): String = {
    val s = new StringBuilder
    var nodeColor = ""
    s ++= "digraph { \n"

    for(n <- pdg.nodes) {
      nodeColor = n.nodeType match  {
        case SourceNode => "[fillcolor=blue, style=filled]"
        case SinkNode   => "[fillcolor=red, style=filled]"
        case _          => ""
      }

      s ++= " " + "\"" + n.show() + "\"" + nodeColor + "\n"
    }

    var edgeNodes = pdg.graph.edges.toOuter

    for (i <- edgeNodes) {
      var x = i.value.label

      var auxStr = ""
      var cont = 0
      for (auxNode <- i) {
        if (cont == 0) {
          auxStr += "\"" + auxNode.show();
        } else {
          auxStr += "\"" + " -> " + "\"" + auxNode.show() + "\"";
        }
        cont += +1
      }

      var xy = x.isInstanceOf[EdgeLabelType]
      if (x.isInstanceOf[EdgeLabelType]) {
        val labelType = x.asInstanceOf[EdgeLabelType].labelType

        if (labelType.toString.equals(TrueLabel.toString)) {
          s ++= " " + auxStr + "[penwidth=3][label=\"T\"]" + "\n"
        } else if (labelType.toString.equals(FalseLabel.toString)) {
          s ++= " " + auxStr + "[penwidth=3][label=\"F\"]" + "\n"
        } else if (labelType.toString.equals(DefLabel.toString)) {
          s ++= " " + auxStr + "[style=dashed, color=black]" + "\n"
        } else {
          s ++= " " + auxStr + "\n"
        }
      } else if (x.isInstanceOf[StringLabel]) {
        s ++= " " + auxStr + "\n"
      }
    }
    s ++= "}"
    return s.toString()
  }

  def createDefEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = graph.StatementCD(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), DefLabel)
  }

}
