package br.ufpe.cin.soot.analysis.jimple

import br.unb.cic.soot.svfa.jimple.{AssignStmt, InvalidStmt, InvokeStmt, JSVFA, Statement}
import soot.PackManager
import soot.jimple._
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.scalar.SimpleLocalDefs
import soot.{Local, Scene, SceneTransformer, SootMethod, Transform}

import java.util


/**
 * A Jimple based implementation of
 * SVFA Analysis with other statements: return and conditional.
 */
abstract class JDFP extends JSVFA{

  val traversedMethodsDF = scala.collection.mutable.Set.empty[SootMethod]

  def buildDFP() {
    svg.enableReturnEdge()
    buildSparseValueFlowGraph()
  }

  override def buildSparseValueFlowGraph() {
    configureSoot()
    beforeGraphConstruction()
    val (pack1, t1) = createSceneTransform() //createSceneTransform for SVFA
    val (pack2, t2) = createSceneTransformDFP() //createSceneTransformDFP for DFP: add conditional and return statement

    PackManager.v().getPack(pack1).add(t1)
    PackManager.v().getPack(pack2).add(t2)

    configurePackages().foreach(p => PackManager.v().getPack(p).apply())

    afterGraphConstruction()
  }

  def createSceneTransformDFP(): (String, Transform) = ("wjtp", new Transform("wjtp.dfp", new TransformerDFP()))

  class TransformerDFP extends SceneTransformer {
    override def internalTransform(phaseName: String, options: util.Map[String, String]): scala.Unit = {
      pointsToAnalysis = Scene.v().getPointsToAnalysis
      Scene.v().getEntryPoints.forEach(method => {
        traverseDFP(method)
        methods = methods + 1
      })
    }
  }

  def traverseDFP(method: SootMethod, forceNewTraversal: Boolean = false) : scala.Unit = {
    if((!forceNewTraversal) && (method.isPhantom || traversedMethodsDF.contains(method))) {
      return
    }

    traversedMethodsDF.add(method)

    val body  = retrieveActiveBodySafely(method)
    val graph = new ExceptionalUnitGraph(body)
    val defs  = new SimpleLocalDefs(graph)

    if (body != null){
      body.getUnits.forEach(unit => {
        try{
          val v = Statement.convert(unit)

          v match {
            case IfStmt(base) => traverse(IfStmt(base), method, defs) //if statment
            case ReturnStmt(base) => traverse(ReturnStmt(base), method, defs) //return
            case _ =>
          }

        }catch {
          case e: Exception => return
        }
      })
    }

  }

  def retrieveActiveBodySafely(method: SootMethod) : soot.Body = {
    try {
      return method.retrieveActiveBody()
    } catch {
      case e: RuntimeException => return null
    }
  }

  case class IfStmt(b: soot.Unit) extends Statement(b) {
    val stmt = base.asInstanceOf[soot.jimple.IfStmt]
  }

  object Statement {
    def convert(base: soot.Unit): Statement =
      if(base.isInstanceOf[soot.jimple.AssignStmt]) {
        AssignStmt(base)
      }
      else if(base.isInstanceOf[soot.jimple.InvokeStmt]) {
        InvokeStmt(base)
      }else if(base.isInstanceOf[soot.jimple.IfStmt]) {
        IfStmt(base)
      }else if(base.isInstanceOf[soot.jimple.ReturnStmt]) {
        ReturnStmt(base)
      }
      else InvalidStmt(base)
  }

  def traverse(stmt: IfStmt, method: SootMethod, defs: SimpleLocalDefs) : scala.Unit = {
    addEdgesFromIfStmt(stmt.base, method, defs)
  }

  def addEdgesFromIfStmt(sourceStmt: soot.Unit, method: SootMethod, defs: SimpleLocalDefs) = {

    //Add useBoxes used in if statement
    sourceStmt.getUseAndDefBoxes.forEach(useBox => {
      if (useBox.getValue.isInstanceOf[Local]) {
        val local = useBox.getValue.asInstanceOf[soot.Local]
        copyRule(sourceStmt, local, method, defs)
      }
    })

  }

  case class ReturnStmt(b: soot.Unit) extends Statement(b) {
    val stmt = base.asInstanceOf[soot.jimple.ReturnStmt]
  }


  def traverse(stmt: ReturnStmt, method: SootMethod, defs: SimpleLocalDefs) : scala.Unit = {
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
}
