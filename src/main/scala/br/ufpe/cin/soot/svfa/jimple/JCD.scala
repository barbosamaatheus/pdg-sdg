package br.ufpe.cin.soot.svfa.jimple

import br.ufpe.cin.soot.graph._
import br.ufpe.cin.soot.svfa.SourceSinkDef
import soot.jimple._
import soot.toolkits.graph.MHGPostDominatorsFinder
import soot.{PackManager, Scene, SceneTransformer, SootMethod, Transform}

import java.util


/**
 * A Jimple based implementation of
 * Control Dependence Analysis.
 */
trait JCD extends SootConfiguration with SourceSinkDef {

  val allocationSitesCD = scala.collection.mutable.HashMap.empty[soot.Value, soot.Unit]
  var cd = new br.ufpe.cin.soot.graph.Graph()
  val traversedMethodsCD = scala.collection.mutable.Set.empty[SootMethod]
  var methods = 0
  def runInFullSparsenessMode() = true

  def buildCD() {
    configureSoot()

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
    if((!forceNewTraversal) && (method.isPhantom || traversedMethodsCD.contains(method))) {
      return
    }

    traversedMethodsCD.add(method)

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
    val label = if (typeEdge) (createTrueEdgeLabel(s, t, method)) else (createFalseEdgeLabel(s, t, method))

    if (s.isInstanceOf[UnitDummy]) {
      if (t.isInstanceOf[UnitDummy]) {
        addEdgeControlDependence(createDummyNode(s, method), createDummyNode(t, method), label)
      }else{
        addEdgeControlDependence(createDummyNode(s, method), createNode(method, t), label)
      }
    }else{
      if (t.isInstanceOf[UnitDummy]) {
        addEdgeControlDependence(createNode(method, s),  createDummyNode(t, method), label)
      }else{
        addEdgeControlDependence(createNode(method, s), createNode(method, t), label)

      }
    }

  }

  def createNode(method: SootMethod, stmt: soot.Unit): StatementNode =
    cd.createNode(method, stmt, analyze)


  def containsNodeCD(node: StatementNode): StatementNode = {
    for (n <- cd.edges()){
      var xx = n.from.asInstanceOf[StatementNode]
      var yy = n.to.asInstanceOf[StatementNode]
      if (xx.equals(node)) return n.from.asInstanceOf[StatementNode]
      if (yy.equals(node)) return n.to.asInstanceOf[StatementNode]
    }
    return null
  }

  def addEdgeControlDependence(source: GraphNode, target: GraphNode, label: EdgeLabel): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      var xy = containsNodeCD(source.asInstanceOf[StatementNode])
      var xx = containsNodeCD(target.asInstanceOf[StatementNode])
      if (xy != null){
        if (xx != null){
          cd.addEdge(xy, xx, label)
        }else{
          cd.addEdge(xy, target.asInstanceOf[StatementNode], label)
        }

      }else{
        if (xx != null) {
          cd.addEdge(source, xx, label)
        }else{
          cd.addEdge(source, target, label)
        }
      }
      res = true
    }
    return res
  }


  def createDummyNode(unit: soot.Unit, method: SootMethod): StatementNode = {
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

  def createEntryPointNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, "Entry Point", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStartNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, "Start", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStopNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, "Stop", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createTrueEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabel = {
    val statement = br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    TrueLabelType(TrueLabel)
  }

  def createFalseEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabel = {
    val statement = br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    FalseLabelType(FalseLabel)
  }

  def createNodeCD(method: SootMethod, stmt: soot.Unit): StatementNode =
    StatementNode(br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, stmt.toString, stmt.getJavaSourceStartLineNumber), analyze(stmt))

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

    s  ++= "\n"

    for (e <- cd.edges) {
      val edge = "\"" + e.from.show() + "\"" + " -> " + "\"" + e.to.show() + "\""
      var l = e.label
      val label: String = e.label match {
        case c: CallSiteLabel =>  {
          if (c.labelType == CallSiteOpenLabel) { "[label=\"cs(\"]" }
          else { "[label=\"cs)\"]" }
        }
        case c: TrueLabelType =>{ "[penwidth=3][label=\"T\"]" }
        case c: FalseLabelType => { "[penwidth=3][label=\"F\"]" }
        case c: DefLabelType => { "[style=dashed, color=black]" }
        case _ => ""
      }
      s ++= " " + edge + " " + label + "\n"
    }
    s ++= "}"
    return s.toString()
  }

}
