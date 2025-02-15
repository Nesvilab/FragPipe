/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package test.org.nesvilab.fragpipe.graph;

import org.nesvilab.utils.IOUtils;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.SimpleGraph;
import org.jgrapht.traverse.DepthFirstIterator;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;
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
//    g.addEdge(v3, v4);
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

  @Test
  public void forYed() throws IOException {
    List<String> lines = IOUtils.readAllLines(new ByteArrayInputStream(fpTaskGraph().getBytes()));

    final Pattern re = Pattern.compile("(cmd[a-zA-Z0-9]+)");

    List<String> nodes = Seq.seq(lines).flatMap(s -> {
      Matcher m = re.matcher(s);
      List<String> names = new ArrayList<>();
      while (m.find()) {
        names.add(m.group(1));
      }
      return Seq.seq(names);
    }).distinct().toList();

//    List<Tuple2<String, String>> edges = Seq.seq(lines).flatMap(s -> {
//      Matcher m = re.matcher(s);
//      List<String> names = new ArrayList<>();
//      while (m.find()) {
//        names.add(m.group(1));
//      }
//      List<Tuple2<String,String>> pairs = new ArrayList<>();
//      for (int i = 1; i < names.size(); i++) {
//        pairs.add(new Tuple2<>(names.get(0), names.get(i)));
//      }
//      return Seq.seq(pairs);
//    }).toList();

    List<Tuple2<String, String>> edges = Seq.seq(lines).map(line -> {
      Matcher m = re.matcher(line);
      List<String> found = new ArrayList<>();
      while (m.find())
        found.add(m.group(1));
      return found;
    })
        .filter(list -> list.size() > 1)
        .flatMap(strings -> {
          Seq<Tuple2<String, String>> s = Seq.empty();
          for (int i = 1; i < strings.size(); i++)
            s = s.append(new Tuple2<>(strings.get(0), strings.get(i)));
          return s;
        })
        .toList();

    System.out.println("Cmd");
    System.out.println(Seq.seq(nodes).toString("\n"));
    System.out.println();
    System.out.println();
    System.out.println();
    System.out.println();
    System.out.println("Source\tTarget");
    System.out.println(Seq.seq(edges).map(tuple -> tuple.v2 + "\t" + tuple.v1).toString("\n"));
  }

  private String fpTaskGraph() {
    return "addToDepGraph(g, cmdStart);\n"
        + "    addToDepGraph(g, cmdUmpire, cmdStart);\n"
        + "    addToDepGraph(g, cmdMsfragger, cmdStart, cmdUmpire);\n"
        + "    addToDepGraph(g, cmdCrystalc, cmdMsfragger);\n"
        + "    addToDepGraph(g, cmdPeptideProphet, cmdMsfragger, cmdCrystalc);\n"
        + "    addToDepGraph(g, cmdProteinProphet, cmdPeptideProphet);\n"
        + "    addToDepGraph(g, cmdPhilosopherDbAnnotate, cmdProteinProphet);\n"
        + "    addToDepGraph(g, cmdPhilosopherFilter, cmdPhilosopherDbAnnotate);\n"
        + "    addToDepGraph(g, cmdFreequant, cmdPhilosopherFilter);\n"
        + "    addToDepGraph(g, cmdIprophet, cmdPhilosopherReport, cmdPeptideProphet);\n"
        + "    addToDepGraph(g, cmdPhilosopherAbacus, cmdPhilosopherReport, cmdIprophet, cmdProteinProphet);\n"
        + "    addToDepGraph(g, cmdIonquant, cmdFreequant);\n"
        + "    addToDepGraph(g, cmdTmtFreequant, cmdPhilosopherFilter, cmdIonquant);\n"
        + "    addToDepGraph(g, cmdTmtLabelQuant, cmdPhilosopherFilter, cmdTmtFreequant);\n"
        + "    addToDepGraph(g, cmdPhilosopherReport, cmdPhilosopherFilter, cmdFreequant, cmdTmtFreequant, cmdTmtLabelQuant);\n"
        + "    addToDepGraph(g, cmdTmt, cmdTmtFreequant, cmdTmtLabelQuant, cmdPhilosopherReport, cmdPhilosopherAbacus);\n"
        + "    addToDepGraph(g, cmdPtmshepherd, cmdTmt);\n"
        + "    addToDepGraph(g, cmdSpecLibGen, cmdPtmshepherd);\n"
        + "    addToDepGraph(g, cmdPhiCleanInit, cmdStart);\n"
        + "    addToDepGraph(g, cmdPhiClean, cmdStart, cmdSpecLibGen);\n";
  }
}
