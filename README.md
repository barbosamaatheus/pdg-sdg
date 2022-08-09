# PDG (Program Dependence Graph Analysis) implementation based on Soot

This is a scala implementation of a framework that builds a pdg flow graph using Soot.

## Status

   * experimental

## Usage

   * clone this repository or download an stable release
   * you will need to add a github token to your **~/.gitconfig**.
     ```
     [github]
             token = TOKEN
     ```
   * build this project using sbt (`sbt compile test`)
   * publish the artifact as a JAR file in your m2 repository (`sbt publish`)
   * create a dependency to the pdg-sdg artifact in your maven project. 

```{xml}
<dependency>
  <groupId>br.ufpe.cin</groupId>
  <artifactId>pdg-sdg_2.12</artifactId>
  <version>0.3.0</version>
</dependency>
```

* Create a class with definition of sink and source tagging.
* Create a class main to use the concrete class below.
* Create a concrete DFP class, implement a class that extends the JDFP class. Add an attribute with the created definitions, you must provide implementations to the following methods:
* `getEntryPoints()` to set up the "main" methods. This implementation must return a list of Soot methods
* `sootClassPath()` to set up the soot classpath. This implementation must return a string
* `analyze(unit)` to identify the type of a node (source, sink, simple node) in the graph; given a statement (soot unit)
* Create a concrete CD class, implement a class that extends the JCD class, and do the implementations of the previous step.
* Create a concrete PDG class, implement a class that extends the JPDG class, and do the implementations of the previous step. When calling PDG analysis, call CD and DFP with the same definitions and to call the buildPDG(cd, dfp) method.

## Dependencies

This project use some of the [FlowDroid](https://github.com/secure-software-engineering/FlowDroid) test cases. The FlowDroid test cases in `src/test/java/securibench` are under [LGPL-2.1](https://github.com/secure-software-engineering/FlowDroid/blob/develop/LICENSE) license.

## UML Class Diagram

See the UML Class Diagram [here](/UML Diagram.pdf).