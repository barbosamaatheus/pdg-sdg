package br.ufpe.cin.soot.graph

import scalax.collection.edge.LkDiEdge

import scala.collection.immutable.HashSet

/*
  * This trait define the base type for node classifications.
  * A node can be classified as SourceNode, SinkNode or SimpleNode.
  */

sealed trait EdgeType

case object SimpleEdge extends EdgeType { def instance: SimpleEdge.type = this }
case object TrueEdge extends EdgeType { def instance: TrueEdge.type = this }
case object FalseEdge extends EdgeType { def instance: FalseEdge.type = this }
case object LoopEdge extends EdgeType { def instance: LoopEdge.type = this }
case object DefEdge extends EdgeType { def instance: DefEdge.type = this }

trait LambdaLabel {
  type T
  var value: T
  val edgeType: EdgeType
}

object EdgeType {
  def convert(edge: String): EdgeType = {
    if(edge.equals(TrueEdge.toString)) {
      TrueEdge
    } else if (edge.equals(FalseEdge.toString)) {
      FalseEdge
    }else if (edge.equals(LoopEdge.toString)){
      LoopEdge
    }else if (edge.equals(DefEdge.toString)){
      DefEdge
    }
    else SimpleEdge
  }
}


/*
  * Simple class to hold all the information needed about a statement,
  * this value is stored in the value attribute of the GraphNode. For the most cases,
  * it is enough for the analysis, but for some situations, something
  * specific for Jimple or Shimple abstractions can be a better option.
  */

case class StmtDef(className: String, method: String, stmt: String, line: Int)

case class StatementCD(className: String, method: String, stmt: String, line: Int, sootUnit: soot.Unit = null, sootMethod: soot.SootMethod = null)


/*
  * This trait define the base for all other labels classifications, like the NodeType
  * the LabelType is used to inform things relevant for the analysis like context sensitive
  * regions or field sensitive actions (store or load).
  */


sealed trait FieldSensitiveLabelType extends LabelType
case object FieldSensitiveStoreLabel extends FieldSensitiveLabelType { def instance: FieldSensitiveStoreLabel.type = this }
case object FieldSensitiveLoadLabel extends FieldSensitiveLabelType { def instance: FieldSensitiveLoadLabel.type = this }

case object LoopLabel extends LabelType { def instance: LoopLabel.type = this }
case object TrueLabel extends LabelType { def instance: TrueLabel.type = this }
case object FalseLabel extends LabelType { def instance: FalseLabel.type = this }
case object DefLabel extends LabelType { def instance: DefLabel.type = this }

case class StringLabelCD(label: String) extends LambdaLabel {
  override type T = String
  override var value: String = label
  override val edgeType: EdgeType = EdgeType.convert(label)
}

case class StatementNodeCD(stmt: StmtDef, stmtType: NodeType) extends GraphNode {
  override type T = StmtDef
  override val value: StmtDef = stmt
  override val nodeType: NodeType = stmtType

  override def show(): String = value.stmt
  override def method(): soot.SootMethod = null
  override def unit(): soot.Unit = null

  //  override def show(): String = "(" ++ value.method + ": " + value.stmt + " - " + value.line + " <" + nodeType.toString + ">)"

  override def toString: String =
    "Node(" + value.method + "," + value.stmt + "," + value.line.toString + "," + nodeType.toString + ")"

  override def equals(o: Any): Boolean = {
    o match {
      case stmt: StatementNode => stmt.value == value && stmt.nodeType == nodeType
      case _ => false
    }
  }

  override def hashCode(): Int = 2 * stmt.hashCode() + nodeType.hashCode()
}


/*
  * Like the graph nodes, the edge labels can be customized and this trait
  * define the abstraction needed to possibility the customization,
  * acting as a container to hold the labels data inside the value attribute.
  */

case class ContextStatement(statement: StatementCD, unit: soot.Unit)

case class EdgeLabelType(context: ContextStatement, labelType: LabelType) extends EdgeLabel {
  override type T = ContextStatement
  override var value = context

  override def equals(o: Any): Boolean = {
    o match {
      case label: EdgeLabelType =>
        return value == label.value && labelType == label.labelType
      case _ => false
    }
  }
}

case class FieldReference(className: String, field: String)

case class FieldSensitiveLabel(fieldRef: FieldReference, labelType: FieldSensitiveLabelType) extends EdgeLabel {
  override type T = FieldReference
  override var value = fieldRef

  def matchFieldReference(otherLabel: Any): Boolean = {
    otherLabel match {
      case defaultFRLabel: FieldSensitiveLabel =>
        val frLabel = defaultFRLabel.value
        // Match store with load or load with store
        if (labelType != defaultFRLabel.labelType) {
          return value == frLabel
        } else {
          false
        }
      case _ => false
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case frLabel: FieldSensitiveLabel =>
        return value == frLabel.value && labelType == frLabel.labelType
      case _ => false
    }
  }
}


case class GraphEdgeCD(from: GraphNode, to: GraphNode, label: EdgeLabel)

class GraphLabelKey() {
  val graph = scalax.collection.mutable.Graph.empty[GraphNode, LkDiEdge]

  var fullGraph: Boolean = false
  var allPaths: Boolean = false
  var optimizeGraph: Boolean = false

  def gNode(outerNode: GraphNode): graph.NodeT = graph.get(outerNode)
  def gEdge(outerEdge: LkDiEdge[GraphNode]): graph.EdgeT = graph.get(outerEdge)

  def contains(node: GraphNode): Boolean = {
    val graphNode = graph.find(node)
    if (graphNode.isDefined) {
      return true
    }
    return false
  }

  def addNode(node: GraphNode): Unit = {
    graph.add(node)
  }

  //  def addEdge(source: GraphNode, target: GraphNode): Unit = {
  //    val label = new StringLabel("Normal")
  //    addEdge(source, target, label)
  //  }

  def addEdge(source: GraphNode, target: GraphNode, label: EdgeLabel): Unit = {
//    if(source == target) {
//      return
//    }

    implicit val factory = scalax.collection.edge.LkDiEdge
    graph.addLEdge(source, target)(label)
  }

  def getAdjacentNodes(node: GraphNode): Option[Set[GraphNode]] = {
    if (contains(node)) {
      return Some(gNode(node).diSuccessors.map(_node => _node.toOuter))
    }
    return None
  }


  def getIgnoredNodes(): HashSet[GraphNode] = {
    var ignoredNodes = HashSet.empty[GraphNode]
    var hasChanged = 51
    while (hasChanged > 50) {
      hasChanged = 0
      this.nodes().diff(ignoredNodes).foreach(n => {
        val gNode = this.gNode(n)
        val hasValidSuccessors = gNode.diSuccessors
          .exists(gSuccessor => !ignoredNodes(gSuccessor.toOuter))
        val hasValidPredecessors = gNode.diPredecessors
          .exists(gPredecessor => !ignoredNodes(gPredecessor.toOuter))

        if (hasValidSuccessors || hasValidPredecessors) {
          n.nodeType match {
            case SourceNode =>
              if (! hasValidSuccessors) {
                ignoredNodes = ignoredNodes + n
                hasChanged += 1
              }
            case SinkNode =>
              if (! hasValidPredecessors) {
                ignoredNodes = ignoredNodes + n
                hasChanged += 1
              }
            case _ =>
              if (!(hasValidPredecessors && hasValidSuccessors)) {
                ignoredNodes = ignoredNodes + n
                hasChanged += 1
              }
          }
        } else {
          ignoredNodes = ignoredNodes + n
          hasChanged += 1
        }
      })
    }

    return ignoredNodes
  }

  def findPathsFullGraph(): List[List[GraphNode]] = {
    val ignoredNodes = getIgnoredNodes()

    if (this.optimizeGraph) {
      ignoredNodes.foreach(node => this.graph.remove(node))
      println("Optimize: " + ignoredNodes.size + " removed nodes.")
    }

    var sourceNodes = HashSet.empty[GraphNode]
    var sinkNodes = HashSet.empty[GraphNode]
    this.nodes().diff(ignoredNodes).foreach(n => {
      n.nodeType match {
        case SourceNode => sourceNodes = sourceNodes + n
        case SinkNode => sinkNodes = sinkNodes + n
        case _ => ()
      }
    })

    val maxConflictsNumber = sinkNodes.size
    var paths = List.empty[List[GraphNode]]
    for(sourceNode <- sourceNodes; sinkNode <- sinkNodes) {
      if (paths.size >= maxConflictsNumber)
        return paths


      val foundedPaths = findPathsOP(sourceNode, sourceNode, sinkNode, HashSet(sourceNode), ignoredNodes, maxConflictsNumber)
      val validPaths = foundedPaths.filter(path => isValidPath(sourceNode, sinkNode, path))
      if (validPaths.nonEmpty)
        paths = paths ++ List(validPaths.head)
    }

    return paths
  }

  def findPathsOP(sourceNode: GraphNode, currentNode: GraphNode, sinkNode: GraphNode, visitedNodes: HashSet[GraphNode],
                  ignoredNodes: HashSet[GraphNode], maxConflictsNumber: Int): List[List[GraphNode]] = {
    var paths = List.empty[List[GraphNode]]

    var validSuccessors = this.gNode(currentNode).diSuccessors
      .map(gSuccessor => gSuccessor.toOuter)

    if (this.optimizeGraph) {
      validSuccessors = validSuccessors.diff(ignoredNodes)
    }

    validSuccessors = validSuccessors.diff(visitedNodes)

    if (allPaths) {
      validSuccessors.foreach(successor => {
        if (paths.size < maxConflictsNumber) {
          if (successor == sinkNode) {
            val path = (visitedNodes + successor).toList
            paths = paths ++ List(path)
          } else {
            val foundedPaths =
              findPathsOP(sourceNode, successor, sinkNode, visitedNodes + successor, ignoredNodes, maxConflictsNumber)
            paths = paths ++ foundedPaths
          }
        }
      })
    } else {
      if (validSuccessors.contains(sinkNode)) {
        val path = (visitedNodes + sinkNode).toList
        if (isValidPath(sourceNode, sinkNode, path)) {
          paths = paths ++ List(path)
        }
      } else {
        validSuccessors.foreach(successor => {
          if (paths.size < maxConflictsNumber) {
            val foundedPaths =
              findPathsOP(sourceNode, successor, sinkNode, visitedNodes + successor, ignoredNodes, maxConflictsNumber)
            paths = paths ++ foundedPaths
          }
        })
      }
    }

    return paths
  }

  def isValidPath(sourceNode: GraphNode, sinkNode: GraphNode, path: List[GraphNode]): Boolean = {
    val gPath = this.gNode(sourceNode)
      .withSubgraph(node => path.contains(node.toOuter))
      .pathTo(this.gNode(sinkNode))
    return isValidPath(gPath.get)
  }

  def findPath(source: GraphNode, target: GraphNode): List[List[GraphNode]] = {
    val fastPath = gNode(source).pathTo(gNode(target))
    val findAllConflictPaths = false

    if (! findAllConflictPaths && fastPath.isDefined && isValidPath(fastPath.get)) {
      return List(fastPath.get.nodes.map(node => node.toOuter).toList)
    }

    val pathBuilder = graph.newPathBuilder(gNode(source))
    val paths = findPaths(source, target, HashSet[GraphNode](), pathBuilder, List())
    val validPaths = paths.filter(path => isValidPath(path))
    return validPaths.map(path => path.nodes.map(node => node.toOuter).toList)
  }

  def findPaths(source: GraphNode, target: GraphNode, visited: HashSet[GraphNode],
                currentPath: graph.PathBuilder, paths: List[graph.Path]): List[graph.Path] = {
    // TODO: find some optimal way to travel in graph
    val adjacencyList = gNode(source).diSuccessors.map(_node => _node.toOuter)
    val auxAdj = gNode(source).diSuccessors
    if (adjacencyList.contains(target) ) {
      currentPath += gNode(target)
      //      return paths ++ List(currentPath.result)
      return List(currentPath.result)
    }

    adjacencyList.foreach(next => {
      if (! visited(next)) {
        var nextPath = currentPath
        nextPath += gNode(next)
        return findPaths(next, target, visited + next, nextPath, paths)
      }
    })
    return List()

    //    var possiblePaths = paths
    //    adjacencyList.foreach(next => {
    //      if (! visited(next)) {
    //        var nextPath = currentPath
    //        nextPath += gNode(next)
    //        val possiblePath = findPaths(next, target, visited + next, nextPath, paths)
    //        if (possiblePath.nonEmpty) {
    //          var findAllConflictPaths = false
    //          if (findAllConflictPaths) {
    //            possiblePaths = possiblePaths ++ possiblePath
    //          } else {
    //            return possiblePath
    //          }
    //        }
    //      }
    //    })
    //
    //    return possiblePaths
  }

  def getUnmatchedCallSites(source: List[CallSiteLabel], target: List[CallSiteLabel]): List[CallSiteLabel] = {
    var unvisitedTargets = target
    var unmatched = List.empty[CallSiteLabel]

    // verify if exists cs) edges without a (cs
    // or if exists (cs edges without a cs)
    source.foreach(s => {
      var matchedCS = List.empty[CallSiteLabel]
      var unmatchedCS = List.empty[CallSiteLabel]
      unvisitedTargets.foreach(label => {
        if (label.matchCallStatement(s))
          matchedCS = matchedCS ++ List(label)
        else
          unmatchedCS = unmatchedCS ++ List(label)
      })

      if (matchedCS.size > 0) {
        unvisitedTargets = unmatchedCS ++ matchedCS.init
      } else {
        unvisitedTargets = unmatchedCS
        unmatched = unmatched ++ List(s)
      }
    })

    return unmatched
  }

  def getUnmatchedFieldReferences(source: List[FieldSensitiveLabel], target: List[FieldSensitiveLabel]): List[FieldSensitiveLabel] = {
    var unmatched = List.empty[FieldSensitiveLabel]
    var unvisitedTargets = target

    source.foreach(s => {
      var matchedFR = List.empty[FieldSensitiveLabel]
      var unmatchedFR = List.empty[FieldSensitiveLabel]
      unvisitedTargets.foreach(label => {
        if (label.matchFieldReference(s))
          matchedFR = matchedFR ++ List(label)
        else
          unmatchedFR = unmatchedFR ++ List(label)
      })

      if (matchedFR.size > 0) {
        unvisitedTargets = unmatchedFR ++ matchedFR.init
      } else {
        unvisitedTargets = unmatchedFR
        unmatched = unmatched ++ List(s)
      }
    })

    return unmatched
  }

  def isValidPath(path: graph.Path): Boolean = {
    var csOpen = List.empty[CallSiteLabel]
    var csClose = List.empty[CallSiteLabel]
    var fsStore = List.empty[FieldSensitiveLabel]
    var fsLoad = List.empty[FieldSensitiveLabel]


    // Filter the labels by type
    path.edges.foreach(edge => {
      val label = edge.toOuter.label

      label match {
        case l: CallSiteLabel => {
          if (l.labelType == CallSiteOpenLabel)
            csOpen = csOpen ++ List(l)
          else
            csClose = csClose ++ List(l)
        }
        case l: FieldSensitiveLabel => {
          if (l.labelType == FieldSensitiveLoadLabel)
            fsLoad = fsLoad ++ List(l)
          else
            fsStore = fsStore ++ List(l)
        }
        case _ => {}
      }
    })

    // Get all the cs) without a (cs
    val unopenedCS = getUnmatchedCallSites(csClose, csOpen)
    // Get all the cs) without a (cs
    val unclosedCS = getUnmatchedCallSites(csOpen, csClose)

    // verify if the unopened and unclosed call-sites are not for the same method
    var matchedUnopenedUnclosedCSCalleeMethod: List[(CallSiteLabel, CallSiteLabel)] = List()
    unclosedCS.foreach(_csOpen => {
      unopenedCS.filter(label => label.matchCalleeMethod(_csOpen)).foreach(_csClose => {
        matchedUnopenedUnclosedCSCalleeMethod = matchedUnopenedUnclosedCSCalleeMethod ++ List((_csOpen, _csClose))
      })
    })

    // Get all the stores without a load
    val unmatchedStores = getUnmatchedFieldReferences(fsStore, fsLoad)
    // Get all the loads without a store
    val unmatchedLoads = getUnmatchedFieldReferences(fsLoad, fsStore)

    val validCS = unopenedCS.isEmpty || unclosedCS.isEmpty || matchedUnopenedUnclosedCSCalleeMethod.isEmpty

    val validFieldRefs = unmatchedLoads.isEmpty || unmatchedStores.isEmpty
    val valid = validCS && validFieldRefs

    return validCS && validFieldRefs
  }

  def nodes(): scala.collection.Set[GraphNode] = graph.nodes.map(node => node.toOuter).toSet

  def edges(): scala.collection.Set[GraphEdgeCD] = graph.edges.map(edge => {
    val from = edge._1.toOuter
    val to = edge._2.toOuter
    val label = edge.toOuter.label.asInstanceOf[EdgeLabel]

    GraphEdgeCD(from, to, label)
  }).toSet

  def numberOfNodes(): Int = graph.nodes.size

  def numberOfEdges(): Int = graph.edges.size
}
