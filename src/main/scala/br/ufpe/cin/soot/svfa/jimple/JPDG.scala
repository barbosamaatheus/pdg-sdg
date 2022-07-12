package br.ufpe.cin.soot.svfa.jimple

import br.ufpe.cin.soot.graph.{CallSiteLabel, CallSiteOpenLabel, DefLabel, DefLabelType, EdgeLabel, FalseLabelType, GraphNode, SinkNode, SourceNode, StatementNode, TrueLabelType}
import soot.jimple._
import soot.options.Options
import soot.toolkits.graph.{ExceptionalBlockGraph}
import soot.{PackManager, Scene, SceneTransformer, SootMethod, Transform}
import java.util


/**
 * A Jimple based implementation of
 * Control Dependence Analysis.
 */
abstract class JPDG extends JCD with JDFP   {

  val allocationSitesPDG = scala.collection.mutable.HashMap.empty[soot.Value, soot.Unit]
  val traversedMethodsPDG = scala.collection.mutable.Set.empty[SootMethod]
  var listDef : List[(AssignStmt, StatementNode, Int)] = List()
  var pdg = new br.ufpe.cin.soot.graph.Graph()
  var hashSetUnit = new util.HashSet[(StatementNode, StatementNode, EdgeLabel)]

  def buildPDG() {

    buildDFP() //svg
    buildCD()  //cd

    mergeDFPAndCD() //pdg

    Options.v().setPhaseOption("jb", "use-original-names:true")

    val (pack, t) = createSceneTransformPDG()
    PackManager.v().getPack(pack).add(t)
    configurePackages().foreach(p => PackManager.v().getPack(p).apply())

    afterGraphConstruction()
  }

  def mergeDFPAndCD(): Unit = {

    //Add df+ edges in pdg

    for (e <- svg.edges()) {
      val from = e.from
      val label = e.label
      val to = e.to
      addNodeAndEdgePDG(from.asInstanceOf[StatementNode], to.asInstanceOf[StatementNode], label)
    }

    //Add cd edges in pdg

    for (e <- cd.edges()) {
      val from = e.from
      val label = e.label
      val to = e.to
      addNodeAndEdgePDG(from.asInstanceOf[StatementNode], to.asInstanceOf[StatementNode], label)
    }

  }

  def addNodeAndEdgePDG(from: StatementNode, to: StatementNode, label: EdgeLabel): Unit = {
    var auxNodeFrom = containsNodePDG(from)
    var auxNodeTo = containsNodePDG(to)
    if (auxNodeFrom != null){
      if (auxNodeTo != null){
        pdg.addEdge(auxNodeFrom, auxNodeTo, label)
      }else{
        pdg.addEdge(auxNodeFrom, to, label)
      }
    }else{
      if (auxNodeTo != null) {
        pdg.addEdge(from, auxNodeTo, label)
      }else{
        pdg.addEdge(from, to, label)
      }
    }
  }

  def containsNodePDG(node: StatementNode): StatementNode = {
    for (n <- pdg.edges()){
      var nodeFrom = n.from.asInstanceOf[StatementNode]
      var nodeTo = n.to.asInstanceOf[StatementNode]
      if (nodeFrom.equals(node)) return n.from.asInstanceOf[StatementNode]
      if (nodeTo.equals(node)) return n.to.asInstanceOf[StatementNode]
    }
    return null
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
          var op1 = listDef(i)._1.stmt.getLeftOp
          var op2 = listDef(j)._1.stmt.getLeftOp
          if (op1.toString().equals(op2.toString())){

            try {
              var auxNodeFrom = containsNodePDG(listDef(i)._2.asInstanceOf[StatementNode])
              var auxNodeTo = containsNodePDG(listDef(j)._2.asInstanceOf[StatementNode])

              val nextI = pdg.getAdjacentNodes(auxNodeFrom).get
              val nextJ = pdg.getAdjacentNodes(auxNodeTo).get

              for (n <- nextI){
                for (m <- nextJ){
                  if (n.equals(m)){
                    val label = createDefEdgeLabel(listDef(i)._1.stmt, listDef(j)._1.stmt, method)

                    if (auxNodeFrom != null){
                      if (auxNodeTo != null){
                        pdg.addEdge(auxNodeFrom, auxNodeTo, label)
                      }else{
                        pdg.addEdge(auxNodeFrom, nextJ.asInstanceOf[StatementNode], label)
                      }
                    }else {
                      if (auxNodeTo != null) {
                        pdg.addEdge(nextI.asInstanceOf[StatementNode], auxNodeTo, label)
                      } else {
                        pdg.addEdge(nextI.asInstanceOf[StatementNode], nextJ.asInstanceOf[StatementNode], label)
                      }
                    }
                  }
                }
              }
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

  def addDefEdge(source: GraphNode, target: GraphNode, label: EdgeLabel): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {

      addNodeAndEdgePDG(source.asInstanceOf[StatementNode], target.asInstanceOf[StatementNode], label)

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

  def createDefEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): DefLabelType = {
    val statement = br.ufpe.cin.soot.graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    DefLabelType(DefLabel)
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

      s ++= " " + "\"" + n.show()+"\"" + nodeColor + "\n"
    }

    s  ++= "\n"

    for (e <- pdg.edges) {
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
