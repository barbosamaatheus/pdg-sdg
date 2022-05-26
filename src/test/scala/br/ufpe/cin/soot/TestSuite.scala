package br.ufpe.cin.soot

import br.ufpe.cin.soot.basic.{Basic11Test, Basic16Test}
import br.ufpe.cin.soot.graph.GraphEdge
import br.ufpe.cin.soot.pdg.PDGDataSetTest
import org.scalatest.{BeforeAndAfter, FunSuite, Ignore}
import samples.FieldSample
import scalax.collection.GraphEdge
import scalax.collection.GraphEdge.{DiEdge, ~>}
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.LkDiEdge
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

class TestSuite extends FunSuite with BeforeAndAfter {

  ignore("we should find exactly three conflicts in this analysis") {
    val svfa = new ArrayTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 3)
  }

  test("we should correctly compute the number of nodes and edges in the BlackBoardTest sample") {
    val svfa = new BlackBoardTest( Array (5), Array (8))
//    val svfa = new BlackBoardTest()
    svfa.buildFlowGraph()
    println(svfa.pdg.nodes.size)
    println(svfa.pdg.numberOfEdges())

    println(svfa.reportConflictsPDG().size)
    println(svfa.pdgToDotModel())

//    assert(svfa.svg.nodes.size == 8)
//    assert(svfa.svg.numberOfEdges() == 11)
//    assert(svfa.reportConflicts().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG1 sample") {
    //    val pdg = new PDG1Test( Array (7), Array (9))
    val pdg = new PDG1Test()
    pdg.buildFlowGraph()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 21)
    assert(pdg.reportConflicts().size == 0)
  }

  test("we should correctly compute the number of nodes and edges in the PDG2 sample") {
    //    val pdg = new PDG2Test( Array (7), Array (9))
    val pdg = new PDG2Test()
    pdg.buildFlowGraph()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 20)
    assert(pdg.reportConflicts().size == 0)
  }

  test("we should correctly compute the number of nodes and edges in the PDG3 sample") {
    //    val pdg = new PDG3Test( Array (7), Array (9))
    val pdg = new PDG3Test()
    pdg.buildFlowGraph()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 13)
    assert(pdg.reportConflicts().size == 0)
  }

  test("we should correctly compute the number of nodes and edges in the PDG4 sample") {
    //    val pdg = new PDG4Test( Array (7), Array (9))
    val pdg = new PDG4Test()
    pdg.buildFlowGraph()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 9)
    assert(pdg.reportConflicts().size == 0)
  }


  test("we should correctly compute the number of nodes and edges in the PDG5 sample") {
    //    val pdg = new PDG5Test( Array (7), Array (9))
    val pdg = new PDG5Test()
    pdg.buildFlowGraph()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 10)
    assert(pdg.reportConflicts().size == 0)
  }

  test("running pdg sprintboot") {
    val pdg = new PDGDataSetTest()
    pdg.buildFlowGraph()
    println(pdg.cd.nodes.size)
    println(pdg.cd.numberOfEdges())

    println(pdg.reportConflictsCD().size)
    println(pdg.cdToDotModel())

   }

  test("we should not find any conflict in the BlackBoardTest sample") {
    val svfa = new BlackBoardTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 0)
  }

  ignore("we should correctly compute the number of nodes and edges of the CC16Test sample") {
    val svfa = new CC16Test()
    svfa.buildFlowGraph()
    assert(svfa.svg.nodes.size == 13)
    assert(svfa.svg.numberOfEdges() == 14)
  }

  test("we should find exactly one conflict of the CC16Test sample") {
    val svfa = new CC16Test()
    svfa.buildFlowGraph()
    println(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size == 1)
  }

  ignore("we should correctly compute the number of nodes and edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildFlowGraph()
    assert(svfa.svg.nodes.size == 17)
  }

  ignore("we should correctly compute the number of edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildFlowGraph()
    assert(svfa.svg.numberOfEdges() == 18)
  }

  test("we should find exactly one conflict in this analysis of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find two conflicts in the LogbackSampleTest analysis") {
    val svfa = new LogbackSampleTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 4)
  }

  test("we should find exactly one conflict in the StringBuggerTest analysis") {
    val svfa = new StringBufferTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the InitStringBufferTest analysis") {
    val svfa = new InitStringBufferTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  ignore("we should find exactly one conflict in the StringConcatTest analysis") {
    val svfa = new StringConcatTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 6)
  }

//  test("we should find exactly one conflict in the StringGetCharsTest analysis") {
//    val svfa = new StringGetCharsTest()
//    svfa.buildSparseValueFlowGraph()
//    println(svfa.svgToDotModel())
//    assert(svfa.reportConflicts().size == 1)
//  }

  test("we should find exactly one conflict in the StringToStringTest analysis") {
    val svfa = new StringToStringTest()
    svfa.buildFlowGraph()

    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly two conflicts in the basic.Basic11 analysis") {
    val svfa = new Basic11Test()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 2)
  }

  test("we should find exactly one conflicts in the basic.Basic16 analysis") {
    val svfa = new Basic16Test()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the ContextSensitiveSample  analysis") {
    val svfa = new ContextSensitiveTest()
    svfa.buildFlowGraph()
    // println(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size == 1)
  }

  ignore("we should find exactly one conflict in the FieldSample analysis") {
    val svfa = new FieldTest()
    svfa.buildFlowGraph()
    assert(svfa.reportConflicts().size == 1)   // NOTE: We are not traversing the body of
                                               //       a method associated to a SinkNode. 
  }

  // This is the case with fields that the source method
  // changes the field that is subsequently used by a sink line
  ignore("we should find exactly one conflict in the MethodFieldTest analysis") {
    val svfa = new MethodFieldTest()
    svfa.buildFlowGraph()
    System.out.println(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size >= 1)
  }

  // This is a simple case that the with a local variable would be detected
  // but with the field variable it isn't detected
  ignore("we should find exactly one conflict in the InvokeInstanceMethodOnFieldTest analysis") {
    val svfa = new InvokeInstanceMethodOnFieldTest()
    svfa.buildFlowGraph()
    System.out.println(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size >= 1)
  }

  // This case is representative of the problem with abstract classes and interfaces
  // that causes the analysis to not be able to infer the concrete implementation of the
  // methods.
  ignore("we should find exactly one conflict in the HashmapTest analysis") {
    val svfa = new HashmapTest()
    svfa.buildFlowGraph()
    System.out.println(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size >= 1)
  }

}
