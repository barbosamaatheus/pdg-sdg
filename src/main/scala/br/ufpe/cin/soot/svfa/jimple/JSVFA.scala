package br.ufpe.cin.soot.svfa.jimple

import br.ufpe.cin.soot.graph
import br.ufpe.cin.soot.graph.{CallSiteCloseLabel, CallSiteLabel, CallSiteOpenLabel, ContextSensitiveRegion, ContextStatement, DefLabel, EdgeLabel, EdgeLabelType, FalseEdge, FalseLabel, FieldReference, FieldSensitiveLabel, FieldSensitiveLoadLabel, FieldSensitiveStoreLabel, LambdaNode, LoopEdge, LoopLabel, SimpleNode, SinkNode, SourceNode, StatementNode, StmtDef, StringLabel, TrueEdge, TrueLabel, UnitDummy, UnitGraphNodes}
import br.ufpe.cin.soot.svfa.{SVFA, SourceSinkDef}
import br.ufpe.cin.soot.svfa.jimple.dsl.{DSL, LanguageParser}
import br.ufpe.cin.soot.svfa.jimple.rules.RuleAction

import java.util
import com.typesafe.scalalogging.LazyLogging
import soot.jimple._
import soot.jimple.internal.{JArrayRef, JAssignStmt}
import soot.jimple.spark.pag
import soot.jimple.spark.pag.{AllocNode, PAG, StringConstantNode}
import soot.jimple.spark.sets.{DoublePointsToSet, HybridPointsToSet, P2SetVisitor}
import soot.toolkits.graph.{BlockGraph, ExceptionalBlockGraph, ExceptionalUnitGraph, LoopNestTree, MHGPostDominatorsFinder}
import soot.toolkits.scalar.SimpleLocalDefs
import soot.{ArrayType, Local, Scene, SceneTransformer, SootField, SootMethod, Transform, jimple}

import scala.collection.mutable.ListBuffer



/**
  * A Jimple based implementation of
  * SVFA.
  */
abstract class JSVFA extends SVFA with Analysis with FieldSensitiveness with SourceSinkDef with LazyLogging  with DSL   {


  var methods = 0
  val traversedMethods = scala.collection.mutable.Set.empty[SootMethod]
  val allocationSites = scala.collection.mutable.HashMap.empty[soot.Value, soot.Unit]
  val arrayStores = scala.collection.mutable.HashMap.empty[Local, List[soot.Unit]]
  val languageParser = new LanguageParser(this)
  var listDef : List[(AssignStmt, StatementNode, Int)] = List()

  val methodRules = languageParser.evaluate(code())

  /*
   * Create an edge  from the definition of the local argument
   * to the definitions of the base object of a method call. In
   * more details, we should use this rule to address a situation
   * like:
   *
   * - virtualinvoke r3.<java.lang.StringBuffer: java.lang.StringBuffer append(java.lang.String)>(r1);
   *
   * Where we wanto create an edge from the definitions of r1 to
   * the definitions of r3.
   */
  trait CopyFromMethodArgumentToBaseObject extends RuleAction {
    def from: Int

    def apply(sootMethod: SootMethod, invokeStmt: jimple.Stmt, localDefs: SimpleLocalDefs) = {
      val srcArg = invokeStmt.getInvokeExpr.getArg(from)
      val expr = invokeStmt.getInvokeExpr
      if(hasBaseObject(expr) && srcArg.isInstanceOf[Local]) {
        val local = srcArg.asInstanceOf[Local]

        val base = getBaseObject(expr)

        if(base.isInstanceOf[Local]) {
          val localBase = base.asInstanceOf[Local]
          localDefs.getDefsOfAt(local, invokeStmt).forEach(sourceStmt => {
            val sourceNode = createNode(sootMethod, sourceStmt)
            localDefs.getDefsOfAt(localBase, invokeStmt).forEach(targetStmt =>{
              val targetNode = createNode(sootMethod, targetStmt)
              updateGraph(sourceNode, targetNode)
            })
          })
        }
      }
    }
  }

  private def getBaseObject(expr: InvokeExpr) =
    if (expr.isInstanceOf[VirtualInvokeExpr])
      expr.asInstanceOf[VirtualInvokeExpr].getBase
    else if(expr.isInstanceOf[SpecialInvokeExpr])
      expr.asInstanceOf[SpecialInvokeExpr].getBase
    else
      expr.asInstanceOf[InstanceInvokeExpr].getBase


  private def hasBaseObject(expr: InvokeExpr) =
    (expr.isInstanceOf[VirtualInvokeExpr] || expr.isInstanceOf[SpecialInvokeExpr] || expr.isInstanceOf[InterfaceInvokeExpr])


  /*
     * Create an edge from a method call to a local.
     * In more details, we should use this rule to address
     * a situation like:
     *
     * - $r6 = virtualinvoke r3.<java.lang.StringBuffer: java.lang.String toString()>();
     *
     * Where we want to create an edge from the definitions of r3 to
     * this statement.
     */
  trait CopyFromMethodCallToLocal extends RuleAction {
    def apply(sootMethod: SootMethod, invokeStmt: jimple.Stmt, localDefs: SimpleLocalDefs) = {
      val expr = invokeStmt.getInvokeExpr
      if(hasBaseObject(expr) && invokeStmt.isInstanceOf[jimple.AssignStmt]) {
        val base = getBaseObject(expr)
        val local = invokeStmt.asInstanceOf[jimple.AssignStmt].getLeftOp
        if(base.isInstanceOf[Local] && local.isInstanceOf[Local]) {
          val localBase = base.asInstanceOf[Local]
          localDefs.getDefsOfAt(localBase, invokeStmt).forEach(source => {
            val sourceNode = createNode(sootMethod, source)
            val targetNode = createNode(sootMethod, invokeStmt)
            updateGraph(sourceNode, targetNode)
          })
        }
      }
    }
  }


  /* Create an edge from the definitions of a local argument
   * to the assignment statement. In more details, we should use this rule to address
   * a situation like:
   * $r12 = virtualinvoke $r11.<java.lang.StringBuilder: java.lang.StringBuilder append(java.lang.String)>(r6);
   */
  trait CopyFromMethodArgumentToLocal extends RuleAction {
    def from: Int

    def apply(sootMethod: SootMethod, invokeStmt: jimple.Stmt, localDefs: SimpleLocalDefs) = {
      val srcArg = invokeStmt.getInvokeExpr.getArg(from)
      if(invokeStmt.isInstanceOf[JAssignStmt] && srcArg.isInstanceOf[Local]) {
        val local = srcArg.asInstanceOf[Local]
        val targetStmt = invokeStmt.asInstanceOf[jimple.AssignStmt]
        localDefs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
          val source = createNode(sootMethod, sourceStmt)
          val target = createNode(sootMethod, targetStmt)
          updateGraph(source, target)
        })
      }
    }
  }

  /*
 * Create an edge between the definitions of the actual
 * arguments of a method call. We should use this rule
 * to address situations like:
 *
 * - System.arraycopy(l1, _, l2, _)
 *
 * Where we wanto to create an edge from the definitions of
 * l1 to the definitions of l2.
 */
  trait CopyBetweenArgs extends RuleAction {
    def from: Int
    def target : Int

    def apply(sootMethod: SootMethod, invokeStmt: jimple.Stmt, localDefs: SimpleLocalDefs) = {
      val srcArg = invokeStmt.getInvokeExpr.getArg(from)
      val destArg = invokeStmt.getInvokeExpr.getArg(target)
      if (srcArg.isInstanceOf[Local] && destArg.isInstanceOf[Local]) {
        localDefs.getDefsOfAt(srcArg.asInstanceOf[Local], invokeStmt).forEach(sourceStmt => {
          val sourceNode = createNode(sootMethod, sourceStmt)
          localDefs.getDefsOfAt(destArg.asInstanceOf[Local], invokeStmt).forEach(targetStmt => {
            val targetNode = createNode(sootMethod, targetStmt)
            updateGraph(sourceNode, targetNode)
          })
        })
      }
    }
  }


  def createSceneTransform(): (String, Transform) = ("wjtp", new Transform("wjtp.svfa", new Transformer()))

  def configurePackages(): List[String] = List("cg", "wjtp")

  def beforeGraphConstruction(): Unit = { }

  def afterGraphConstruction() { }

  def initAllocationSites(): Unit = {
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
              allocationSites += (right -> unit)
            }
          }
        })
      }
    }
  }

  class Transformer extends SceneTransformer {
    override def internalTransform(phaseName: String, options: util.Map[String, String]): Unit = {
      pointsToAnalysis = Scene.v().getPointsToAnalysis
      initAllocationSites()
      Scene.v().getSootClassPath
      Scene.v().getEntryPoints.forEach(method => {
        traverse(method)
        methods = methods + 1
      })
    }
  }

  def traverse(method: SootMethod, forceNewTraversal: Boolean = false) : Unit = {
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

    val graph = new ExceptionalUnitGraph(body)
    val defs  = new SimpleLocalDefs(graph)

    body.getUnits.forEach(unit => {
      try{
        val v = Statement.convert(unit)

        v match {
          case AssignStmt(base) => traverse(AssignStmt(base), method, defs)
          case InvokeStmt(base) => traverse(InvokeStmt(base), method, defs)
          case IfStmt(base) => traverse(IfStmt(base), method, defs) //if statment
          case ReturnStmt(base) => traverse(ReturnStmt(base), method, defs) //return
          case _ if analyze(unit) == SinkNode => traverseSinkStatement(v, method, defs)
          case _ =>
        }

      }catch {
        case e: Exception => return
      }
    })

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

  def traverse(assignStmt: AssignStmt, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    val left = assignStmt.stmt.getLeftOp
    val right = assignStmt.stmt.getRightOp

    (left, right) match {
      case (p: Local, q: InstanceFieldRef) => loadRule(assignStmt.stmt, q, method, defs)
      case (p: Local, q: ArrayRef) => loadArrayRule(assignStmt.stmt, q, method, defs)
      case (p: Local, q: InvokeExpr) => invokeRule(assignStmt, q, method, defs)
      case (p: Local, q: Local) => copyRule(assignStmt.stmt, q, method, defs)
      case (p: Local, _) => copyRuleInvolvingExpressions(assignStmt.stmt, method, defs)
      case (p: InstanceFieldRef, _: Local) => storeRule(assignStmt.stmt, p, method, defs)
      case (p: JArrayRef, _) => storeArrayRule(assignStmt)
      case _ =>
    }
  }

  def traverse(stmt: ReturnStmt, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    val op = stmt.stmt.getUseBoxes

    op.forEach(useBox => {
      (useBox.getValue) match {
        case (q: InstanceFieldRef) => loadRule(stmt.stmt, q, method, defs)
        case (q: ArrayRef) => loadArrayRule(stmt.stmt, q, method, defs)
        case (q: Local) => copyRule(stmt.stmt, q, method, defs)
        case _ =>
      }
    })
  }

  def traverse(stmt: IfStmt, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    addEdgesFromIfStmt(stmt.base, method, defs)
  }

  def addEdgesFromIfStmt(sourceStmt: soot.Unit, method: SootMethod, defs: SimpleLocalDefs) = {

    //Add useBoxes used in if statement
    sourceStmt.getUseAndDefBoxes.forEach(useBox => {
      if (useBox.getValue.isInstanceOf[Local]) {
        val local = useBox.getValue.asInstanceOf[Local]
        copyRule(sourceStmt, local, method, defs)
      }
    })

  }

  def traverseSinkStatement(statement: Statement, method: SootMethod, defs: SimpleLocalDefs): Unit = {
    statement.base.getUseBoxes.forEach(box => {
      box match {
        case local : Local => copyRule(statement.base, local, method, defs)
        case fieldRef : InstanceFieldRef => loadRule(statement.base, fieldRef, method, defs)
        case _ =>
        // TODO:
        //   we have to think about other cases here.
        //   e.g: a reference to a parameter
      }
    })
  }

  def traverse(stmt: InvokeStmt, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    val exp = stmt.stmt.getInvokeExpr
    //if is an object and is not a call method
    exp.getUseBoxes.forEach(useBox => {
      (useBox.getValue) match {
        case (q: InstanceFieldRef) => loadRule(stmt.stmt, q, method, defs)
        case (q: ArrayRef) => loadArrayRule(stmt.stmt, q, method, defs)
        case (q: Local) => copyRule(stmt.stmt, q, method, defs)
        case _ =>
      }
    })
    invokeRule(stmt, exp, method, defs)
  }

  def addDefEdges(s: soot.Unit, t: soot.Unit, method: SootMethod): Unit = {
    if (s.isInstanceOf[GotoStmt] || t.isInstanceOf[GotoStmt]) return
    var source = createNode(method, s)
    var target = createNode(method, t)

    val auxLabel = createDefEdgeLabel(s, t, method)

    addLoopEdge(source, target, auxLabel)
  }

  def addControlDependenceEdge(s: soot.Unit, t: soot.Unit, typeEdge: Boolean, method: SootMethod): Unit = {
    if (s.isInstanceOf[GotoStmt] || t.isInstanceOf[GotoStmt]) return
    var source = createNode(method, s)
    var target = createNode(method, t)

    if (s.isInstanceOf[UnitDummy]) {
      source = createDummyNode(s, method)
    }

    if (t.isInstanceOf[UnitDummy]){
      target = createDummyNode(t, method)
    }
    val label = if (typeEdge) (createTrueEdgeLabel(s, t, method)) else (createFalseEdgeLabel(s, t, method))

    addEdgeControlDependence(source, target, label)
  }

  def createDummyNode(unit: soot.Unit, method: SootMethod): StatementNode = {
    var node = createNode(method, unit)

    if (unit.toString().contains("EntryPoint")) {
      node = createEntryPointNode(method)
    } else if (unit.toString().contains("Start")) {
      node = createStartNode(method)
    } else if (unit.toString().contains("Stop")) {
      node = createStopNode(method)
    }
    return node
  }

  def addLoopEdge(source: LambdaNode, target: LambdaNode, label: EdgeLabel): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
//      val label = createLoopEdgeLabel()
      pdg.addEdge(source, target, label)
      res = true
    }
    return res
  }

  def addEdgeControlDependence(source: LambdaNode, target: LambdaNode, label: EdgeLabelType): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      svgcd.addEdge(source, target, label)
      pdg.addEdge(source, target, label)
      cd.addEdge(source, target, label)
      res = true
    }
    return res
  }

  def addEdgeSVGCD(source: LambdaNode, target: LambdaNode): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      svgcd.addEdge(source, target)
      pdg.addEdge(source, target)
      res = true
    }
    return res
  }

  def addEdgeSVG(source: LambdaNode, target: LambdaNode): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      svg.addEdge(source, target)
      res = true
    }
    return res
  }


  private def invokeRule(callStmt: Statement, exp: InvokeExpr, caller: SootMethod, defs: SimpleLocalDefs): Unit = {

    var callee = exp.getMethod

    if(analyze(callStmt.base) == SinkNode) {
      defsToCallOfSinkMethod(callStmt, exp, caller, defs)
      return  // TODO: we are not exploring the body of a sink method.
              //       For this reason, we only find one path in the
              //       FieldSample test case, instead of two.
    }

    if(analyze(callStmt.base) == SourceNode) {
      val source = createNode(caller, callStmt.base)
      svg.addNode(source)
    }

     for(r <- methodRules) {
      if(r.check(callee)) {
        r.apply(caller, callStmt.base.asInstanceOf[jimple.Stmt], defs)
        return
      }
    }

    if(intraprocedural()) return

    var pmtCount = 0
    val body = callee.retrieveActiveBody()
    val g = new ExceptionalUnitGraph(body)
    val calleeDefs = new SimpleLocalDefs(g)

    body.getUnits.forEach(s => {
      if(isThisInitStmt(exp, s)) {
        defsToThisObject(callStmt, caller, defs, s, exp, callee)
      }
      else if(isParameterInitStmt(exp, pmtCount, s)) {
        defsToFormalArgs(callStmt, caller, defs, s, exp, callee, pmtCount)
        pmtCount = pmtCount + 1
      }
      else if(isAssignReturnStmt(callStmt.base, s)) {
        defsToCallSite(caller, callee, calleeDefs, callStmt.base, s)
      }
    })

    traverse(callee)
  }

  private def applyPhantomMethodCallRule(callStmt: Statement, exp: InvokeExpr, caller: SootMethod, defs: SimpleLocalDefs) = {
    val srcArg = exp.getArg(0)
    val destArg = exp.getArg(2)
    if (srcArg.isInstanceOf[Local] && destArg.isInstanceOf[Local]) {
      defs.getDefsOfAt(srcArg.asInstanceOf[Local], callStmt.base).forEach(srcArgDefStmt => {
        val sourceNode = createNode(caller, srcArgDefStmt)
        val allocationNodes = findAllocationSites(destArg.asInstanceOf[Local])
        allocationNodes.foreach(targetNode => {
          updateGraph(sourceNode, targetNode)
        })
      })
    }
  }

  /*
     * This rule deals with the following situation:
     *
     * (*) p = q
     *
     * In this case, we create an edge from defs(q)
     * to the statement p = q.
     */
  private def copyRule(targetStmt: soot.Unit, local: Local, method: SootMethod, defs: SimpleLocalDefs) = {
    defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
      val source = createNode(method, sourceStmt)
      val target = createNode(method, targetStmt)
      updateGraph(source, target)
    })
  }

  /*
   * This rule deals with the following situation:
   *
   * (*) p = q + r
   *
   * In this case, we create and edge from defs(q) and
   * from defs(r) to the statement p = q + r
   */
  def copyRuleInvolvingExpressions(stmt: jimple.AssignStmt, method: SootMethod, defs: SimpleLocalDefs) = {
    stmt.getRightOp.getUseBoxes.forEach(box => {
      if(box.getValue.isInstanceOf[Local]) {
        val local = box.getValue.asInstanceOf[Local]
        copyRule(stmt, local, method, defs)
      }
    })
  }

  /*
   * This rule deals with the following situations:
   *
   *  (*) p = q.f
   */
  private def loadRule(stmt: soot.Unit, ref: InstanceFieldRef, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    val base = ref.getBase
    // value field of a string.
    val className = ref.getFieldRef.declaringClass().getName
    if((className == "java.lang.String") && ref.getFieldRef.name == "value") {
      if(base.isInstanceOf[Local]) {
        defs.getDefsOfAt(base.asInstanceOf[Local], stmt).forEach(source => {
          val sourceNode = createNode(method, source)
          val targetNode = createNode(method, stmt)
          updateGraph(sourceNode, targetNode)
        })
      }
      return;
    }
    // default case
    if(base.isInstanceOf[Local]) {
      val allocationNodes = findAllocationSites(base.asInstanceOf[Local], false, ref.getField)
      allocationNodes.foreach(source => {
        val target = createNode(method, stmt)
        updateGraph(source, target)
        svg.getAdjacentNodes(source).get.foreach(s => updateGraph(s, target))
      })

      defs.getDefsOfAt(base.asInstanceOf[Local], stmt).forEach(source => {
        val sourceNode = createNode(method, source)
        val targetNode = createNode(method, stmt)

        val fsLoadLabel = createFieldSensitiveLoadLabel(ref)
        svg.addEdge(sourceNode, targetNode, fsLoadLabel)
        pdg.addEdge(sourceNode, targetNode, fsLoadLabel)
      })

//      if (isFieldSensitiveAnalysis()) {
//        recursivePointsTo(Statement.convert(stmt), method)
//      }
    }
  }

  private def loadArrayRule(targetStmt: soot.Unit, ref: ArrayRef, method: SootMethod, defs: SimpleLocalDefs) : Unit = {
    val base = ref.getBase

    if(base.isInstanceOf[Local]) {
      val local = base.asInstanceOf[Local]

      defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
        val source = createNode(method, sourceStmt)
        val target = createNode(method, targetStmt)
        updateGraph(source, target)
      })

      val stores = arrayStores.getOrElseUpdate(local, List())
      stores.foreach(sourceStmt => {
        val source = createNode(method, sourceStmt)
        val target = createNode(method, targetStmt)
        updateGraph(source, target)
      })
    }
  }

  /*
   * This rule deals with statements in the form:
   *
   * (*) p.f = expression
   */
  private def storeRule(targetStmt: jimple.AssignStmt, fieldRef: InstanceFieldRef, method: SootMethod, defs: SimpleLocalDefs) = {
    val local = targetStmt.getRightOp.asInstanceOf[Local]
    if (fieldRef.getBase.isInstanceOf[Local]) {
      val base = fieldRef.getBase.asInstanceOf[Local]
      if (fieldRef.getField.getDeclaringClass.getName == "java.lang.String" && fieldRef.getField.getName == "value") {
        defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
          val source = createNode(method, sourceStmt)
          val allocationNodes = findAllocationSites(base)
          allocationNodes.foreach(targetNode => {
            updateGraph(source, targetNode)
          })
        })
      }
      else {
        //        val allocationNodes = findAllocationSites(base)


//        val allocationNodes = findAllocationSites(base, true, fieldRef.getField)
//        if(!allocationNodes.isEmpty) {
//          allocationNodes.foreach(targetNode => {
        defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
          val source = createNode(method, sourceStmt)
          val target = createNode(method, targetStmt)
          updateGraph(source, target)

          if (isFieldSensitiveAnalysis()) {
            recursivePointsTo(Statement.convert(targetStmt), method)
          }
        })
        //          })
        //        }
      }
    }
  }

  def storeArrayRule(assignStmt: AssignStmt) {
    val l = assignStmt.stmt.getLeftOp.asInstanceOf[JArrayRef].getBase.asInstanceOf[Local]
    val stores = assignStmt.stmt :: arrayStores.getOrElseUpdate(l, List())
    arrayStores.put(l, stores)
  }

  private def defsToCallSite(caller: SootMethod, callee: SootMethod, calleeDefs: SimpleLocalDefs, callStmt: soot.Unit, retStmt: soot.Unit) = {
    val target = createNode(caller, callStmt)

    val local = retStmt.asInstanceOf[soot.jimple.ReturnStmt].getOp.asInstanceOf[Local]
    calleeDefs.getDefsOfAt(local, retStmt).forEach(sourceStmt => {
      val source = createNode(callee, sourceStmt)
      val csCloseLabel = createCSCloseLabel(caller, callStmt, callee)
      svg.addEdge(source, target)
      pdg.addEdge(source, target)
      cd.addEdge(source, target)

      if(local.getType.isInstanceOf[ArrayType]) {
        val stores = arrayStores.getOrElseUpdate(local, List())
        stores.foreach(sourceStmt => {
          val source = createNode(callee, sourceStmt)
          val csCloseLabel = createCSCloseLabel(caller, callStmt, callee)
          svg.addEdge(source, target)
          pdg.addEdge(source, target)
          cd.addEdge(source, target)
        })
      }
    })

  }

  private def defsToThisObject(callStatement: Statement, caller: SootMethod, calleeDefs: SimpleLocalDefs, targetStmt: soot.Unit, expr: InvokeExpr, callee: SootMethod) : Unit = {
    val invokeExpr = expr match {
      case e: VirtualInvokeExpr => e
      case e: SpecialInvokeExpr => e
      case e: InterfaceInvokeExpr => e
      case _ => null //TODO: not sure if the other cases
                     // are also relevant here. Otherwise,
                     // we can just match with InstanceInvokeExpr
    }

    if(invokeExpr != null) {
      if(invokeExpr.getBase.isInstanceOf[Local]) {

        val target = createNode(callee, targetStmt)

        val base = invokeExpr.getBase.asInstanceOf[Local]
        calleeDefs.getDefsOfAt(base, callStatement.base).forEach(sourceStmt => {
          val source = createNode(caller, sourceStmt)
          val csOpenLabel = createCSOpenLabel(caller, callStatement.base, callee)
          svg.addEdge(source, target, csOpenLabel)
        })
      }
    }
  }

  private def defsToFormalArgs(stmt: Statement, caller: SootMethod, defs: SimpleLocalDefs, assignStmt: soot.Unit, exp: InvokeExpr, callee: SootMethod, pmtCount: Int) = {
    val target = createNode(callee, assignStmt)

    val local = exp.getArg(pmtCount).asInstanceOf[Local]
    defs.getDefsOfAt(local, stmt.base).forEach(sourceStmt => {
      val source = createNode(caller, sourceStmt)
      val csOpenLabel = createCSOpenLabel(caller, stmt.base, callee)
      svg.addEdge(source, target, csOpenLabel)
    })
  }

  private def defsToCallOfSinkMethod(stmt: Statement, exp: InvokeExpr, caller: SootMethod, defs: SimpleLocalDefs) = {
    // edges from definitions to args
    exp.getArgs.stream().filter(a => a.isInstanceOf[Local]).forEach(a => {
      val local = a.asInstanceOf[Local]
      val targetStmt = stmt.base
      defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
        val source = createNode(caller, sourceStmt)
        val target = createNode(caller, targetStmt)
        updateGraph(source, target)
//
//        if(isFieldSensitiveAnalysis()){
//          recursivePointsTo(Statement.convert(sourceStmt), caller)
//        }
      })

      if(local.getType.isInstanceOf[ArrayType]) {
        val stores = arrayStores.getOrElseUpdate(local, List())
        stores.foreach(sourceStmt => {
          val source = createNode(caller, sourceStmt)
          val target = createNode(caller, targetStmt)
          updateGraph(source, target)
        })
      }
    })
    // edges from definition to base object of an invoke expression
    if(isFieldSensitiveAnalysis() && exp.isInstanceOf[InstanceInvokeExpr]) {
      if(exp.asInstanceOf[InstanceInvokeExpr].getBase.isInstanceOf[Local]) {
        val local = exp.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
        val targetStmt = stmt.base
        defs.getDefsOfAt(local, targetStmt).forEach(sourceStmt => {
          val source = createNode(caller, sourceStmt)
          val target = createNode(caller, targetStmt)
          updateGraph(source, target)
        })
      }
    }
  }

  private def recursivePointsTo(stmt: Statement, caller: SootMethod): Unit = {
    if (stmt.isInstanceOf[AssignStmt]) {
      val assign = stmt.asInstanceOf[AssignStmt].stmt

      // On store => q.f = p
      val leftOp = assign.getLeftOp
      if (leftOp.isInstanceOf[InstanceFieldRef]) {
        val fieldRef = leftOp.asInstanceOf[InstanceFieldRef]
        val ptsBase = findAllocationSites(fieldRef.getBase.asInstanceOf[Local])
        ptsBase.foreach(target => {
          val source = createNode(caller, stmt.base)
//          updateGraph(source, target)

          val fsStoreLabel = createFieldSensitiveStoreLabel(fieldRef)
          svg.addEdge(source, target, fsStoreLabel)
        })
      }

      // On load => p = q.f
      val rightOp = assign.getRightOp
      if (rightOp.isInstanceOf[InstanceFieldRef]) {
        val fieldRef = rightOp.asInstanceOf[InstanceFieldRef]
        val ptsRight = findAllocationSites(rightOp.asInstanceOf[InstanceFieldRef].getBase.asInstanceOf[Local])
        ptsRight.foreach(source => {
          val target = createNode(caller, stmt.base)
          //          updateGraph(source, target)

          val fsLoadLabel = createFieldSensitiveLoadLabel(fieldRef)
          svg.addEdge(source, target, fsLoadLabel)

//          svg.getAdjacentNodes(source).get.foreach(s => {
////            updateGraph(s, target)
//              svg.addEdge(s, target, fsLoadLabel)
//          })
        })
      }
    }
  }


  /*
   * creates a graph node from a sootMethod / sootUnit
   */

  def createEntryPointNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Entry Point", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStartNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Start", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createStopNode(method: SootMethod): StatementNode = {
    try {
      return new StatementNode(StmtDef(method.getDeclaringClass.toString, method.getSignature, "Stop", 0), SimpleNode)
    } catch {
      case e: NullPointerException => {
        println ("Error creating node, an invalid statement.")
        return null
      }
    }
  }

  def createNode(method: SootMethod, stmt: soot.Unit): StatementNode =
    StatementNode(StmtDef(method.getDeclaringClass.toString, method.getSignature, stmt.toString, stmt.getJavaSourceStartLineNumber), analyze(stmt))

  def createCSOpenLabel(method: SootMethod, stmt: soot.Unit, callee: SootMethod): CallSiteLabel = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, stmt.toString, stmt.getJavaSourceStartLineNumber)
    CallSiteLabel(ContextSensitiveRegion(statement, callee.toString), CallSiteOpenLabel)
  }

  def createCSCloseLabel(method: SootMethod, stmt: soot.Unit, callee: SootMethod): CallSiteLabel = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, stmt.toString, stmt.getJavaSourceStartLineNumber)
    CallSiteLabel(ContextSensitiveRegion(statement, callee.toString), CallSiteCloseLabel)
  }

  def createLoopEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), LoopLabel)
  }

  def createTrueEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), TrueLabel)
  }

  def createFalseEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), FalseLabel)
  }

  def createDefEdgeLabel(source: soot.Unit, target: soot.Unit, method: SootMethod): EdgeLabelType = {
    val statement = graph.Statement(method.getDeclaringClass.toString, method.getSignature, source.toString, source.getJavaSourceStartLineNumber)
    EdgeLabelType(ContextStatement(statement, target), DefLabel)
  }

  def createFieldSensitiveStoreLabel(fieldRef: InstanceFieldRef): FieldSensitiveLabel = {
    val declaringClass = fieldRef.getField.getDeclaringClass.getName
    val fieldName = fieldRef.getField.getName
    new FieldSensitiveLabel(FieldReference(declaringClass, fieldName), FieldSensitiveStoreLabel)
  }

  def createFieldSensitiveLoadLabel(fieldRef: InstanceFieldRef): FieldSensitiveLabel = {
    val declaringClass = fieldRef.getField.getDeclaringClass.getName
    val fieldName = fieldRef.getField.getName
    new FieldSensitiveLabel(FieldReference(declaringClass, fieldName), FieldSensitiveLoadLabel)
  }

  def isThisInitStmt(expr: InvokeExpr, unit: soot.Unit) : Boolean =
    unit.isInstanceOf[IdentityStmt] && unit.asInstanceOf[IdentityStmt].getRightOp.isInstanceOf[ThisRef]

  def isParameterInitStmt(expr: InvokeExpr, pmtCount: Int, unit: soot.Unit) : Boolean =
    unit.isInstanceOf[IdentityStmt] && unit.asInstanceOf[IdentityStmt].getRightOp.isInstanceOf[ParameterRef] && expr.getArg(pmtCount).isInstanceOf[Local]

  def isAssignReturnStmt(callSite: soot.Unit, unit: soot.Unit) : Boolean =
   unit.isInstanceOf[soot.jimple.ReturnStmt] && unit.asInstanceOf[soot.jimple.ReturnStmt].getOp.isInstanceOf[Local] &&
     callSite.isInstanceOf[soot.jimple.AssignStmt]

  def findAllocationSites(local: Local, oldSet: Boolean = true, field: SootField = null) : ListBuffer[LambdaNode] = {
    if(pointsToAnalysis.isInstanceOf[PAG]) {
      val pta = pointsToAnalysis.asInstanceOf[PAG]

      val reachingObjects = if(field == null) pta.reachingObjects(local.asInstanceOf[Local])
                            else pta.reachingObjects(local, field)

      if(!reachingObjects.isEmpty) {
        val allocations = if(oldSet) reachingObjects.asInstanceOf[DoublePointsToSet].getOldSet
                          else reachingObjects.asInstanceOf[DoublePointsToSet].getNewSet

        val v = new AllocationVisitor()
        allocations.asInstanceOf[HybridPointsToSet].forall(v)
        return v.allocationNodes
      }
    }
    new ListBuffer[LambdaNode]()
  }

  /*
   * a class to visit the allocation nodes of the objects that
   * a field might point to.
   *
   * @param method method of the statement stmt
   * @param stmt statement with a load operation
   */
  class AllocationVisitor() extends P2SetVisitor {
    var allocationNodes = new ListBuffer[LambdaNode]()
    override def visit(n: pag.Node): Unit = {
      if (n.isInstanceOf[AllocNode]) {
        val allocationNode = n.asInstanceOf[AllocNode]

        var unit: soot.Unit = null

        if (allocationNode.getNewExpr.isInstanceOf[NewExpr]) {
          if (allocationSites.contains(allocationNode.getNewExpr.asInstanceOf[NewExpr])) {
            unit = allocationSites(allocationNode.getNewExpr.asInstanceOf[NewExpr])
          }
        }
        else if(allocationNode.getNewExpr.isInstanceOf[NewArrayExpr]) {
          if (allocationSites.contains(allocationNode.getNewExpr.asInstanceOf[NewArrayExpr])) {
            unit = allocationSites(allocationNode.getNewExpr.asInstanceOf[NewArrayExpr])
          }
        }
//        else if(allocationNode.getNewExpr.isInstanceOf[StringConstant]) {
//          if (allocationSites.contains(allocationNode.getNewExpr.asInstanceOf[StringConstant])) {
//            unit = allocationSites(allocationNode.getNewExpr.asInstanceOf[StringConstant])
//          }
//        }

        if(unit != null) {
          allocationNodes += createNode(allocationNode.getMethod, unit)
        }
      }
    }
  }

  /**
   * Override this method in the case that
   * a complete graph should be generated.
   *
   * Otherwise, only nodes that can be reached from
   * source nodes will be in the graph
   *
   * @return true for a full sparse version of the graph.
   *         false otherwise.
   * @deprecated
   */
   def runInFullSparsenessMode() = true

//  /*
//   * It either updates the graph or not, depending on
//   * the types of the nodes.
//   */
  private def updateGraph(source: LambdaNode, target: LambdaNode, forceNewEdge: Boolean = false): Boolean = {
    var res = false
    if(!runInFullSparsenessMode() || true) {
      svg.addEdge(source, target)
      svgcd.addEdge(source, target)
      pdg.addEdge(source, target)
      res = true
    }



  // this first case can still introduce irrelevant nodes
//    if(svg.contains(source)) {//) || svg.map.contains(target)) {
//      svg.addEdge(source, target)
//      res = true
//    }
//    else if(source.nodeType == SourceNode || source.nodeType == SinkNode) {
//      svg.addEdge(source, target)
//      res = true
//    }
//    else if(target.nodeType == SourceNode || target.nodeType == SinkNode) {
//      svg.addEdge(source, target)
//      res = true
//    }
    return res
  }

}
