package br.ufpe.cin.soot.analysis

import br.ufpe.cin.soot.graph.NodeType

trait SourceSinkDef {
  def analyze(unit: soot.Unit) : NodeType
}
