package br.ufpe.cin.soot.svfa.jimple

import br.ufpe.cin.soot.svfa.SVFA
import br.ufpe.cin.soot.graph.{NodeType, _}
import br.ufpe.cin.soot.svfa.jimple.dsl.{DSL, LanguageParser}
import br.ufpe.cin.soot.svfa.jimple.rules.RuleAction
import br.ufpe.cin.soot.svfa.SourceSinkDef
import com.typesafe.scalalogging.LazyLogging
import jdk.nashorn.internal.runtime.regexp.joni.constants.NodeType
import soot.jimple.{InvokeStmt, _}
import soot.jimple.internal.{JArrayRef, JAssignStmt}
import soot.jimple.spark.ondemand.DemandCSPointsTo
import soot.jimple.spark.pag
import soot.jimple.spark.pag.{AllocNode, PAG}
import soot.jimple.spark.sets.{DoublePointsToSet, HybridPointsToSet, P2SetVisitor}
import soot.options.Options
import soot.toolkits.graph.{ExceptionalUnitGraph, MHGPostDominatorsFinder}
import soot.toolkits.scalar.SimpleLocalDefs
import soot.{ArrayType, Local, PackManager, Scene, SceneTransformer, SootField, SootMethod, Transform, jimple}

import java.util
import scala.collection.mutable.ListBuffer


/**
 * A Jimple based implementation of
 * Control Dependence Analysis.
 */
trait JCD extends JSVFA   {

  val allocationSitesCD = scala.collection.mutable.HashMap.empty[soot.Value, soot.Unit]
  var cd = new br.ufpe.cin.soot.graph.GraphLabelKey()

  def buildCD() {
    configureSoot()
    Options.v().setPhaseOption("jb", "use-original-names:true")
    beforeGraphConstruction()
    val (pack2, t2) = createSceneTransformCD()
    PackManager.v().getPack(pack2).add(t2)
    configurePackages().foreach(p => PackManager.v().getPack(p).apply())

    afterGraphConstruction()
  }
  def createSceneTransformCD(): (String, Transform) = ("wjtp", new Transform("wjtp.cd", new TransformerCD()))

  class TransformerCD extends SceneTransformer {
    override def internalTransform(phaseName: String, options: util.Map[String, String]): Unit = {
      pointsToAnalysis = Scene.v().getPointsToAnalysis
      initAllocationSitesCD()
      Scene.v().getEntryPoints.forEach(method => {
        traverseCD(method)
        methods = methods + 1
      })
    }
  }

  def initAllocationSitesCD(): Unit = {
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
              allocationSitesCD += (right -> unit)
            }
          }
        })
      }
    }
  }

  def traverseCD(method: SootMethod, forceNewTraversal: Boolean = false) : Unit = {
    if((!forceNewTraversal) && (method.isPhantom || traversedMethods.contains(method))) {
      return
    }

    traversedMethods.add(method)

    val body  = method.retrieveActiveBody()

    try {
      val unitGraph= new UnitGraphNodes(body, false)

      val analysis = new MHGPostDominatorsFinder(unitGraph)

      unitGraph.forEach(unit => {
        var edges = unitGraph.getSuccsOf(unit)
        var ADominators = analysis.getDominators(unit)

        //        println(unit, unit.getJavaSourceStartLineNumber())
        //Find a path with from unit to edges, using the post-dominator tree, excluding the LCA node
        //Add True and False edge
        var typeEd = true
        var count = 0
        edges.forEach(unitAux =>{
          var BDominators = analysis.getDominators(unitAux)
          var dItB = BDominators.iterator
          while (dItB.hasNext()) {
            val dsB = dItB.next()
            if (!ADominators.contains(dsB)){
              if (count > 0){
                typeEd = false
              } else {
                typeEd = true //The first time is true
              }
              addControlDependenceEdge(unit, dsB, typeEd, method)
            }
          }
          count = count + 1
        })
      })
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
      }
      case e: Exception => {
        println ("An invalid statement.")
      }
    }

  }

  def addControlDependenceEdge(s: soot.Unit, t: soot.Unit, typeEdge: Boolean, method: SootMethod): Unit = {
    if (s.isInstanceOf[GotoStmt] || t.isInstanceOf[GotoStmt]) return
    var source = createNodeCD(method, s)
    var target = createNodeCD(method, t)

    if (s.isInstanceOf[UnitDummy]) {
      source = createDummyNode(s, method)
    }

    if (t.isInstanceOf[UnitDummy]){
      target = createDummyNode(t, method)
    }
    val label = if (typeEdge) (createTrueEdgeLabel(s, t, method)) else (createFalseEdgeLabel(s, t, method))

    addEdgeControlDependence(source, target, label)
  }

  def addEdgeControlDependence(source: GraphNode, target: GraphNode, label: EdgeLabelType): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      cd.addEdge(source, target, label)
      res = true
    }
    return res
  }


  def createDummyNode(unit: soot.Unit, method: SootMethod): StatementNodeCD = {
    var node = createNodeCD(method, unit)

    if (unit.toString().contains("EntryPoint")) {
      node = createEntryPointNode(method)
    } else if (unit.toString().contains("Start")) {
      node = createStartNode(method)
    } else if (unit.toString().contains("Stop")) {
      node = createStopNode(method)
    }
    return node
  }

  def createEntryPointNode(method: SootMethod): StatementNodeCD = {
    try {
      return new StatementNodeCD(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Entry Point", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStartNode(method: SootMethod): StatementNodeCD = {
    try {
      return new StatementNodeCD(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Start", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStopNode(method: SootMethod): StatementNodeCD = {
    try {
      return new StatementNodeCD(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Stop", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createTrueEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = br.ufpe.cin.soot.graph.StatementCD(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), TrueLabel)
  }

  def createFalseEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = br.ufpe.cin.soot.graph.StatementCD(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), FalseLabel)
  }

  def createNodeCD(method: SootMethod, stmt: soot.Unit): StatementNodeCD =
    StatementNodeCD(StmtDef(method.getDeclaringClass.toString, method.getSignature, stmt.toString, stmt.getJavaSourceStartLineNumber), analyze(stmt))


  def reportConflictsCD(): scala.collection.Set[String] =
    findConflictingPathsCD().map(p => p.toString)


  def findConflictingPathsCD(): scala.collection.Set[List[GraphNode]] = {
    if (cd.fullGraph) {
      val conflicts = cd.findPathsFullGraph()
      return conflicts.toSet
    } else {
      val sourceNodes = cd.nodes.filter(n => n.nodeType == SourceNode)
      val sinkNodes = cd.nodes.filter(n => n.nodeType == SinkNode)

      var conflicts: List[List[GraphNode]] = List()
      sourceNodes.foreach(source => {
        sinkNodes.foreach(sink => {
          val paths = cd.findPath(source, sink)
          conflicts = conflicts ++ paths
        })
      })

      conflicts.filter(p => p.nonEmpty).toSet
    }
  }

  def cdToDotModel(): String = {
    val s = new StringBuilder
    var nodeColor = ""
    s ++= "digraph { \n"

    for(n <- cd.nodes) {
      nodeColor = n.nodeType match  {
        case SourceNode => "[fillcolor=blue, style=filled]"
        case SinkNode   => "[fillcolor=red, style=filled]"
        case _          => ""
      }

      s ++= " " + "\"" + n.show() + "\"" + nodeColor + "\n"
    }

    var edgeNodes = cd.graph.edges.toOuter

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

}
