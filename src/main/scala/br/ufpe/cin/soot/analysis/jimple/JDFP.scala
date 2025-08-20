package br.ufpe.cin.soot.analysis.jimple

import br.unb.cic.soot.graph.VisitedMethods
import br.unb.cic.soot.svfa.jimple.{AssignStmt, InvalidStmt, InvokeStmt, JSVFA, Statement}
import soot.{Local, PackManager, Scene, SceneTransformer, SootMethod, Transform}
import soot.jimple._
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.scalar.SimpleLocalDefs

import java.util
import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.mutable.ListBuffer


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
      getAnalysisEntryPoint().forEach(method => {
        traverseDFP(method, new ListBuffer[VisitedMethods]())
        methods = methods + 1
      })
    }
  }

  def traverseDFP(method: SootMethod, visitedMethods: ListBuffer[VisitedMethods], forceNewTraversal: Boolean = false) : scala.Unit = {
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
          val auxVisitedMethod = new ListBuffer[VisitedMethods]()
          auxVisitedMethod.++=(visitedMethods)
          auxVisitedMethod += new VisitedMethods(method, unit, unit.getJavaSourceStartLineNumber)
          v match {
            case IfStmt(base) => traverse(IfStmt(base), method, defs, visitedMethods) //if statment
            case ReturnStmt(base) => traverse(ReturnStmt(base), method, defs, visitedMethods) //return
            case _ =>
          }

        }catch {
          case e: Exception => return
        }
      })
    }

  }

  def retrieveActiveBodySafely(method: SootMethod) : soot.Body = {
    if (method.retrieveActiveBody() == null) {
      return null
    } else {
      return method.retrieveActiveBody()
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

  def traverse(stmt: IfStmt, method: SootMethod, defs: SimpleLocalDefs, visitedMethods: ListBuffer[VisitedMethods]) : scala.Unit = {
    addEdgesFromIfStmt(stmt.base, method, defs, visitedMethods)
  }

  def addEdgesFromIfStmt(sourceStmt: soot.Unit, method: SootMethod, defs: SimpleLocalDefs, visitedMethods: ListBuffer[VisitedMethods]) = {

    //Add useBoxes used in if statement
    sourceStmt.getUseAndDefBoxes.forEach(useBox => {
      if (useBox.getValue.isInstanceOf[Local]) {
        val local = useBox.getValue.asInstanceOf[soot.Local]
        copyRule(sourceStmt, local, method, defs, visitedMethods)
      }
    })

  }

  case class ReturnStmt(b: soot.Unit) extends Statement(b) {
    val stmt = base.asInstanceOf[soot.jimple.ReturnStmt]
  }

  def getAnalysisEntryPoint(): util.List[SootMethod] = {
    Scene.v().getEntryPoints
  }


  def traverse(stmt: ReturnStmt, method: SootMethod, defs: SimpleLocalDefs, visitedMethods: ListBuffer[VisitedMethods]) : scala.Unit = {
    val op = stmt.stmt.getUseBoxes

    op.forEach(useBox => {
      (useBox.getValue) match {
        case (q: InstanceFieldRef) => loadRule(stmt.stmt, q, method, defs, visitedMethods)
        case (q: ArrayRef) => loadArrayRule(stmt.stmt, q, method, defs, visitedMethods)
        case (q: Local) => copyRule(stmt.stmt, q, method, defs, visitedMethods)
        case _ =>
      }
    })
  }
}
