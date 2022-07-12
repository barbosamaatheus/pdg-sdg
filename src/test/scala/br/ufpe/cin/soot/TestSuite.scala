package br.ufpe.cin.soot

import br.ufpe.cin.soot.basic.{Basic11Test, Basic16Test}
import org.scalatest.{BeforeAndAfter, FunSuite}

class TestSuite extends FunSuite with BeforeAndAfter {

  test("we should find exactly three conflicts in this analysis") {
    val svfa = new ArrayTest(Array(9), Array(12, 13, 14))
    svfa.buildSparseValueFlowGraph()
//    This stack3 there is no edge to use x[1] = stack3
    print(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size == 6)
  }

  test("CD BlackBoardTest sample") {
    val cd = new CDTest( Array (8), Array (11))
    cd.buildCD()

    println(cd.reportConflictsCD().size)
    println(cd.cdToDotModel())
  }

  test("DF+ BlackBoardTest sample") {
    val dfp = new DFPTest( Array (7), Array (8))
    dfp.buildDFP()
    println(dfp.svgToDotModel())
  }

  ignore("PDG BlackBoardTest sample") {
    val pdg = new PDGTest( Array (7), Array (14))
    pdg.buildPDG()
    println(pdg.pdgToDotModel())
  }

  test("we should correctly compute the number of nodes and edges in the PDG1 sample") {
    val pdg = new PDG1Test( Array (7), Array (10))
    pdg.buildPDG()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 21)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes, edges and one conflict (CD) in the PDG1 sample") {
    val pdg = new PDG1Test( Array (7), Array (9))
    pdg.buildPDG()

    println(pdg.pdgToDotModel())
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG2 sample") {
    val pdg = new PDG2Test( Array (6), Array (12))
    pdg.buildPDG()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 20)
    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG3 sample") {
    val pdg = new PDG3Test( Array (10), Array (12))
    pdg.buildPDG()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 13)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG4 sample") {
    val pdg = new PDG4Test( Array (5), Array (10))
    pdg.buildPDG()

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 9)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG5 sample") {
    val pdg = new PDG5Test( Array (10), Array (9))
    pdg.buildPDG()
    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 10)
    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG6 sample") {
    val pdg = new PDG6Test( Array (9), Array (10))
    pdg.buildPDG()
    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 8)
    assert(pdg.reportConflictsPDG().size == 0)
  }

  test("we should correctly compute the number of nodes, edges and one conflict in the PDG6 sample") {
    val pdg = new PDG6Test( Array (7), Array (10))
    pdg.buildPDG()
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG7 sample") {
    val pdg = new PDG7Test( Array (6, 7), Array (9, 11))
    pdg.buildPDG()
    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 8)
    assert(pdg.reportConflictsPDG().size == 2)
  }

  test("we should correctly compute the number of nodes and edges in the PDGSlide sample") {
    val pdg = new PDGTestSlide( Array (9), Array (11))
    pdg.buildPDG()
    println(pdg.pdgToDotModel())

    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the CDSlide sample") {
    val cd = new CDTestSlide( Array (9), Array (11))
    cd.buildCD()
    println(cd.cdToDotModel())

    assert(cd.reportConflictsCD().size >= 1)
  }

  test("we should not find any conflict in the BlackBoardTest sample") {
    val svfa = new BlackBoardTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 0)
  }

  test("we should correctly compute the number of nodes and edges of the CC16Test sample") {
    val svfa = new CC16Test()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.svg.nodes.size == 12)
    assert(svfa.svg.numberOfEdges() == 15)
  }

  test("we should find exactly one conflict of the CC16Test sample") {
    val svfa = new CC16Test()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should correctly compute the number of nodes and edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.svg.nodes.size == 16)
  }

  test("we should correctly compute the number of edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.svg.numberOfEdges() == 19)
  }

  test("we should find exactly one conflict in this analysis of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find two conflicts in the LogbackSampleTest analysis") {
    val svfa = new LogbackSampleTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 3)
  }

  test("we should find exactly one conflict in the StringBuggerTest analysis") {
    val svfa = new StringBufferTest(Array(6), Array(10))
    svfa.buildSparseValueFlowGraph()
    print(svfa.svgToDotModel())
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the InitStringBufferTest analysis") {
    val svfa = new InitStringBufferTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the StringConcatTest analysis") {
    val svfa = new StringConcatTest(Array (6), Array (14, 15, 16, 17, 18, 19, 20))
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size >= 2)
  }

  test("we should find exactly one conflict in the StringToStringTest analysis") {
    val svfa = new StringToStringTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly two conflicts in the basic.Basic11 analysis") {
    val svfa = new Basic11Test()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 2)
  }

  test("we should find exactly one conflicts in the basic.Basic16 analysis") {
    val svfa = new Basic16Test()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the ContextSensitiveSample  analysis") {
    val svfa = new ContextSensitiveTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 1)
  }

  test("we should find exactly one conflict in the FieldSample analysis") {
    val svfa = new FieldTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 2)   // NOTE: We are not traversing the body of
    //       a method associated to a SinkNode.
  }

  // This is the case with fields that the source method
  // changes the field that is subsequently used by a sink line
  test("we should find exactly one conflict in the MethodFieldTest analysis") {
    val svfa = new MethodFieldTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 0)
  }

  // This is a simple case that the with a local variable would be detected
  // but with the field variable it isn't detected
  test("we should find exactly one conflict in the InvokeInstanceMethodOnFieldTest analysis") {
    val svfa = new InvokeInstanceMethodOnFieldTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 0)
  }

  // This case is representative of the problem with abstract classes and interfaces
  // that causes the analysis to not be able to infer the concrete implementation of the
  // methods.
  test("we should find exactly one conflict in the HashmapTest analysis") {
    val svfa = new HashmapTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.reportConflicts().size == 0)
  }
  /*
      test("running dataset scenery") {
        val pdg = new PDGDataSetTest()
        pdg.buildPDG()
        println(pdg.pdg.nodes.size)
        println(pdg.pdg.numberOfEdges())

        println(pdg.reportConflictsPDG().size)
        println(pdg.pdgToDotModel())

       }
  */
}
