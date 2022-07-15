package br.ufpe.cin.soot.svfa

import br.ufpe.cin.soot.graph.{GraphNode, SinkNode, SourceNode}
import soot._

/**
 * Base class for all implementations
 * of SVFA algorithms.
 */
abstract class SVFA extends SootConfiguration {

  var svg = new br.ufpe.cin.soot.graph.Graph()


  def buildSparseValueFlowGraph() {
    configureSoot()
    beforeGraphConstruction()
    val (pack, t) = createSceneTransform()
    PackManager.v().getPack(pack).add(t)
    configurePackages().foreach(p => PackManager.v().getPack(p).apply())
    afterGraphConstruction()
  }



  def findConflictingPaths(): scala.collection.Set[List[GraphNode]] = {
    if (svg.fullGraph) {
      val conflicts = svg.findPathsFullGraph()
      return conflicts.toSet
    } else {
      val sourceNodes = svg.nodes.filter(n => n.nodeType == SourceNode)
      val sinkNodes = svg.nodes.filter(n => n.nodeType == SinkNode)

      //      val conflicts = for(source <- sourceNodes; sink <- sinkNodes)
      //         yield svg.findPath(source, sink)

      var conflicts: List[List[GraphNode]] = List()
      sourceNodes.foreach(source => {
        sinkNodes.foreach(sink => {
          val paths = svg.findPath(source, sink)
          conflicts = conflicts ++ paths
        })
      })

      conflicts.filter(p => p.nonEmpty).toSet
    }
  }


  def reportConflicts(): scala.collection.Set[String] =
    findConflictingPaths().map(p => p.toString)


  def svgToDotModel(): String = {
    svg.toDotModel()
  }


}
