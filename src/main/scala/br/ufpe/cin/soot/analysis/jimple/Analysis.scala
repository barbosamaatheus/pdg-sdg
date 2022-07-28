package br.ufpe.cin.soot.analysis.jimple

trait Analysis {
  def interprocedural() : Boolean

  def intraprocedural() : Boolean = ! interprocedural()
}

trait Interprocedural extends Analysis {
  override def interprocedural = true
}

trait Intraprocedural extends Analysis {
  override def interprocedural() = false
}
