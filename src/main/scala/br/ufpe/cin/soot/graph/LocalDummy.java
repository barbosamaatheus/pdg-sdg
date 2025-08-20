package br.ufpe.cin.soot.graph;

import soot.*;
import soot.baf.Baf;
import soot.jimple.ConvertToBaf;
import soot.jimple.JimpleToBafContext;
import soot.jimple.JimpleValueSwitch;
import soot.util.Numberer;
import soot.util.Switch;

import java.util.Collections;
import java.util.List;

/**
 * Creating a Local to use when creating the Unit
 */
public class LocalDummy implements Local, ConvertToBaf {
     protected String name;
     Type type;

     public LocalDummy(String name, Type type) {
          setName(name);
          setType(type);
     }

     /** Returns true if the given object is structurally equal to this one. */
     @Override
     public boolean equivTo(Object o) {
          return this.equals(o);
     }

     /**
      * Returns a hash code for this object, consistent with structural equality.
      */
     @Override
     public int equivHashCode() {
          final int prime = 31;
          int result = 1;
          result = prime * result + ((name == null) ? 0 : name.hashCode());
          result = prime * result + ((type == null) ? 0 : type.hashCode());
          return result;
     }

     /** Returns a clone of the current JimpleLocal. */
     @Override
     public Object clone() {
          // do not intern the name again
          LocalDummy local = new LocalDummy(null, type);
          local.name = name;
          return local;
     }

     /** Returns the name of this object. */
     @Override
     public String getName() {
          return name;
     }

     /** Sets the name of this object as given. */
     @Override
     public void setName(String name) {
          this.name = (name == null) ? null : name.intern();
     }

     /** Returns the type of this local. */
     @Override
     public Type getType() {
          return type;
     }

     /** Sets the type of this local. */
     @Override
     public void setType(Type t) {
          this.type = t;
     }

     @Override
     public boolean isStackLocal() {
          return true;
     }

     @Override
     public String toString() {
          return getName();
     }

     @Override
     public void toString(UnitPrinter up) {
          up.local(this);
     }

     @Override
     public final List<ValueBox> getUseBoxes() {
          return Collections.emptyList();
     }

     @Override
     public void apply(Switch sw) {
          ((JimpleValueSwitch) sw).caseLocal(this);
     }

     @Override
     public void convertToBaf(JimpleToBafContext context, List<Unit> out) {
          Unit u = Baf.v().newLoadInst(getType(), context.getBafLocalOfJimpleLocal(this));
          u.addAllTagsOf(context.getCurrentUnit());
          out.add(u);
     }

     @Override
     public final int getNumber() {
          return number;
     }

     @Override
     public void setNumber(int number) {
          this.number = number;
     }

     private volatile int number = 0;
}