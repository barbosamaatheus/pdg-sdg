package br.ufpe.cin.soot.graph;

import soot.*;
import soot.baf.ThrowInst;
import soot.jimple.Jimple;
import soot.jimple.JimpleToBafContext;
import soot.jimple.Stmt;
import soot.jimple.ThrowStmt;

import java.util.List;

/**
 * <p>
 * Represents a control flow graph for a {@link Body} instance where the nodes are {@link Unit} instances, and where control
 * flow associated with exceptions is taken into account.
 * </p>
 *
 * <p>
 * To describe precisely the circumstances under which exceptional edges are added to the graph, we need to distinguish the
 * exceptions thrown explicitly by a <code>throw</code> instruction from the exceptions which are thrown implicitly by the VM
 * to signal an error it encounters in the course of executing an instruction, which need not be a <code>throw</code>.
 * </p>
 *
 * <p>
 * For every {@link ThrowInst} or {@link ThrowStmt} <code>Unit</code> which may explicitly throw an exception that would be
 * caught by a {@link Trap} in the <code>Body</code>, there will be an edge from the <code>throw</code> <code>Unit</code> to
 * the <code>Trap</code> handler's first <code>Unit</code>.
 * </p>
 *
 * <p>
 * For every <code>Unit</code> which may implicitly throw an exception that could be caught by a <code>Trap</code> in the
 * <code>Body</code>, there will be an edge from each of the excepting <code>Unit</code>'s predecessors to the
 * <code>Trap</code> handler's first <code>Unit</code> (since any of those predecessors may have been the last
 * <code>Unit</code> to complete execution before the handler starts execution). If the excepting <code>Unit</code> might
 * have the side effect of changing some field, then there will definitely be an edge from the excepting <code>Unit</code>
 * itself to its handlers, since the side effect might occur before the exception is raised. If the excepting
 * <code>Unit</code> has no side effects, then parameters passed to the <code>ExceptionalUnitGraph</code> constructor
 * determine whether or not there is an edge from the excepting <code>Unit</code> itself to the handler <code>Unit</code>.
 * </p>
 *
 * Creating a false statement in Jimple to represent the vertices in the PDG graph: Start, Stop and EntryPoint
 */

public class UnitDummy extends AbstractDummyStmt implements Stmt {
     private String dummyNode;

     public UnitDummy(Value op, String node) {
          this(Jimple.v().newVariableBox(op));
          this.dummyNode = node;
     }

     public String getDummyNode(){
          return this.dummyNode;
     }

     public void setDummyNode(String dummyNode){
          this.dummyNode = dummyNode;
     }

     protected UnitDummy(ValueBox opBox) {
          super(opBox);
     }

     public Object clone() {
          return null;
     }

     public String toString() {
          return this.dummyNode;
     }

     public void toString(UnitPrinter up) {
          up.literal(this.dummyNode);
     }

     public void convertToBaf(JimpleToBafContext context, List<Unit> out) {}

     public boolean fallsThrough() {
          return true;
     }

     public boolean branches() {
          return false;
     }

}