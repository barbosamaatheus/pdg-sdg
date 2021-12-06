package br.ufpe.cin.soot.graph;

import soot.Value;
import soot.ValueBox;
import soot.jimple.internal.AbstractStmt;

@SuppressWarnings("serial")
public abstract class AbstractDummyStmt extends AbstractStmt {

     final ValueBox opBox;

     protected AbstractDummyStmt(ValueBox opBox) {
          this.opBox = opBox;
     }

     final public Value getOp() {
          return opBox.getValue();
     }

     final public void setOp(Value op) {
          opBox.setValue(op);
     }

     final public ValueBox getOpBox() {
          return opBox;
     }

}