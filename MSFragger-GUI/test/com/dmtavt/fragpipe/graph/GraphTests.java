package com.dmtavt.fragpipe.graph;

import java.util.Iterator;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.SimpleGraph;
import org.jgrapht.traverse.DepthFirstIterator;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.junit.Test;

public class GraphTests {

  @Test
  public void stringGraphTests() {
    Graph<String, DefaultEdge> stringGraph = createStringGraph();

    // note undirected edges are printed as: {<v1>,<v2>}
    System.out.println("-- toString output");
    System.out.println(stringGraph.toString());
    System.out.println();

    System.out.println("Traversing graph starting from some node that has number 2 in name");
    String start = stringGraph
        .vertexSet().stream().filter(s -> s.contains("2")).findAny()
        .get();
    Iterator<String> iterator = new DepthFirstIterator<>(stringGraph, start);
    while (iterator.hasNext()) {
      String s= iterator.next();
      System.out.println(s);
    }

  }

  private static Graph<String, DefaultEdge> createStringGraph()
  {
    Graph<String, DefaultEdge> g = new SimpleGraph<>(DefaultEdge.class);

    String v1 = "v1";
    String v2 = "v2";
    String v3 = "v3";
    String v4 = "v4";

    // add the vertices
    g.addVertex(v1);
    g.addVertex(v2);
    //g.addVertex(v3);
    g.addVertex(v4);

    // add edges to create a circuit
    g.addEdge(v1, v2);
    //g.addEdge(v2, v3);
    g.addEdge(v3, v4);
    //g.addEdge(v4, v1);

    return g;
  }

  @Test
  public void stringDagTests() {
    DirectedAcyclicGraph<String, DefaultEdge> dag = createDag();
    TopologicalOrderIterator<String, DefaultEdge> it = new TopologicalOrderIterator<>(
        dag);
    System.out.println("Iterting dag in topo order");
    while(it.hasNext()) {
      System.out.println(" - " + it.next());
    }
  }

  private static DirectedAcyclicGraph<String, DefaultEdge> createDag() {
    DirectedAcyclicGraph<String, DefaultEdge> g = new DirectedAcyclicGraph<>(DefaultEdge.class);
    g.addVertex("v1");
    g.addVertex("v2");
    g.addEdge("v1", "v2");

    g.addVertex("v3");
    g.addEdge("v1", "v3");

    g.addVertex("v4");
    g.addEdge("v3", "v4");

    g.addVertex("v5");
    g.addEdge("v5", "v1");

    g.addVertex("v6");
    g.addEdge("v1", "v6");
    g.addEdge("v4", "v6");

    return g;
  }
}
