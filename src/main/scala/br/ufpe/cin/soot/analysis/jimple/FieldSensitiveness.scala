package br.ufpe.cin.soot.analysis.jimple

trait FieldSensitiveness {
  def isFieldSensitiveAnalysis(): Boolean
}

trait FieldSensitive extends FieldSensitiveness {
  override def isFieldSensitiveAnalysis(): Boolean = true
}

trait FieldInsensitive extends FieldSensitiveness {
  override def isFieldSensitiveAnalysis(): Boolean = false
}
