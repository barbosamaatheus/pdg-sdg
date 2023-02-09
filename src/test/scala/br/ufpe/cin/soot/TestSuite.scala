package br.ufpe.cin.soot

import org.scalatest.{BeforeAndAfter, FunSuite}

class TestSuite extends FunSuite with BeforeAndAfter {

  var jcd = new CDTest()
  var jdfp = new DFPTest()

  test("CD BlackBoardTest sample") {
    val cd = new CDTest( Array (10), Array (12), "samples.BlackBoard", "main")
    cd.buildCD()

    println(cd.cd.reportConflicts().size)
    println(cd.cd.toDotModel())
  }

  test("CD sample returning one conflict") {
    val cd = new CDTest( Array (16), Array (18), "samples.CDExample", "main")
    cd.buildCD()

    println(cd.cd.reportConflicts().size)
    println(cd.cd.toDotModel())

    assert(cd.cd.nodes.size == 9)
    assert(cd.cd.numberOfEdges() == 8)
    assert(cd.reportConflictsCD().size == 1)
  }

  test("DF+ BlackBoardTest sample") {
    val className = "samples.BlackBoard"
    val mainMethod = "main"

    val dfp = new DFPTest( Array (7), Array (8), className, mainMethod)
    dfp.buildDFP()
    println(dfp.svgToDotModel())
  }

  test("PDG BlackBoardTest sample") {
    val className = "samples.BlackBoard"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (7), Array (14), className, mainMethod)
    jcd = new CDTest(Array (7), Array (14), className, mainMethod)
    jdfp = new DFPTest(Array (7), Array (14), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())
  }

  test("we should correctly compute the number of nodes and edges in the PDG1 sample") {
    val className = "samples.PDG1"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (7), Array (10), className, mainMethod)
    jcd = new CDTest(Array (7), Array (10), className, mainMethod)
    jdfp = new DFPTest(Array (7), Array (10), className, mainMethod)
    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 21)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes, edges and one conflict (CD) in the PDG1 sample") {
    val className = "samples.PDG1"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (7), Array (9), className, mainMethod)
    jcd = new CDTest(Array (7), Array (9), className, mainMethod)
    jdfp = new DFPTest(Array (7), Array (9), className, mainMethod)
    pdg.buildPDG(jcd, jdfp)
    println(pdg.pdgToDotModel())
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG2 sample") {
    val className = "samples.PDG2"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (6), Array (12), className, mainMethod)
    jcd = new CDTest(Array (6), Array (12), className, mainMethod)
    jdfp = new DFPTest(Array (6), Array (12), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)
    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 20)
    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG3 sample") {
    val className = "samples.PDG3"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (10), Array (12), className, mainMethod)
    jcd = new CDTest( Array (10), Array (12), className, mainMethod)
    jdfp = new DFPTest( Array (10), Array (12), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 13)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG4 sample") {
    val className = "samples.PDG4"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (5), Array (10), className, mainMethod)
    jcd = new CDTest( Array (5), Array (10), className, mainMethod)
    jdfp = new DFPTest( Array (5), Array (10), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 9)
    assert(pdg.pdg.numberOfEdges() == 9)
    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG5 sample") {
    val className = "samples.PDG5"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (10), Array (9), className, mainMethod)
    jcd = new CDTest( Array (10), Array (9), className, mainMethod)
    jdfp = new DFPTest( Array (10), Array (9), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 10)
    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG6 sample") {
    val className = "samples.PDG6"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (9), Array (10), className, mainMethod)
    jcd = new CDTest( Array (9), Array (10), className, mainMethod)
    jdfp = new DFPTest( Array (9), Array (10), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 8)
    assert(pdg.reportConflictsPDG().size == 0)
  }

  test("we should correctly compute the number of nodes, edges and one conflict in the PDG6 sample") {
    val className = "samples.PDG6"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (7), Array (10), className, mainMethod)
    jcd = new CDTest( Array (7), Array (10), className, mainMethod)
    jdfp = new DFPTest( Array (7), Array (10), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.reportConflictsPDG().size == 1)
  }

  test("we should correctly compute the number of nodes and edges in the PDG7 sample") {
    val className = "samples.PDG7"
    val mainMethod = "main"

    val pdg = new PDGTest( Array (6, 7), Array (9, 11), className, mainMethod)
    jcd = new CDTest( Array (6, 7), Array (9, 11), className, mainMethod)
    jdfp = new DFPTest( Array (6, 7), Array (9, 11), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.pdg.nodes.size == 8)
    assert(pdg.pdg.numberOfEdges() == 8)
    assert(pdg.reportConflictsPDG().size == 2)
  }

  test("we should correctly compute the number of nodes and edges in the SlideSample sample with PDG") {
    val className = "samples.SlideSample"
    val mainMethod = "cleanText"

    val pdg = new PDGTest( Array (13), Array (15), className, mainMethod)
    jcd = new CDTest( Array (13), Array (15), className, mainMethod)
    jdfp = new DFPTest( Array (13), Array (15), className, mainMethod)

    pdg.buildPDG(jcd, jdfp)

    println(pdg.pdgToDotModel())

    assert(pdg.reportConflictsPDG().size >= 1)
  }

  test("we should correctly compute the number of nodes and edges in the CDSlide sample") {
    val className = "samples.SlideSample"
    val mainMethod = "cleanText"

    val cd = new CDTest( Array (13), Array (15), className, mainMethod)

    cd.buildCD()

    println(cd.cd.toDotModel())

    assert(cd.cd.reportConflicts().size >= 1)
  }

  test("we should not find any conflict in the BlackBoardTest sample") {
    val className = "samples.BlackBoard"
    val mainMethod = "main"

    val svfa = new CDTest(Array (10), Array (12), className, mainMethod)
    svfa.buildCD()
    print(svfa.cdToDotModel())
//    assert(svfa.svg.reportConflicts().size == 0)
  }


  test("we should correctly compute the number of nodes and edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.svg.nodes.size == 16)
  }

  test("we should correctly compute the number of edges of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    println(svfa.svgToDotModel())
    assert(svfa.svg.numberOfEdges() == 18)
  }

  test("we should find exactly one conflict in this analysis of the IfElseTest sample") {
    val svfa = new IfElseTest()
    svfa.buildSparseValueFlowGraph()
    assert(svfa.svg.reportConflicts().size == 1)
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
