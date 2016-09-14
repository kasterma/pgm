package net.kasterma.pgm

/**
  * Created by kasterma on 14/09/16.
  */
object typedExper1 {

  class Scope(val scope: Set[Int], val scope_name: String) {
    override def toString: String = scope_name
  }

  class Edge(val edge_name: String, val scope: Scope) {
    override def toString: String = edge_name + " : " + scope.toString
  }

  abstract class Node(val dom: Scope, val ran: Scope, val node_name: String) {
    override def toString: String = node_name + " : " + dom.toString + " -> " + ran.toString
  }

  class DeterministicNode(dom: Scope, ran: Scope, node_name: String, val f: Int => Int) extends Node(dom, ran, node_name) {
    def apply = f
  }

  class RootNode(ran: Scope, node_name: String, val f: Unit => Int)
      extends Node(new Scope(Set.empty, "empty_scope"), ran, node_name) {
    override def toString: String = node_name + ": () -> " + ran.toString
    def apply = f
  }

  class Graph(val edges: List[Edge], val nodes: List[Node], val graph_name: String) {
    override def toString: String = graph_name + ": " + edges.mkString(", ") + " :: " + nodes.mkString(", ")
  }

  def main(args: Array[String]): Unit = {
    val s123 = new Scope(Set(1,2,3), "1to3")
    val xx = new Graph(List.empty, List(new DeterministicNode(s123, s123, "node1", x => 2*x)), "try1")

    println(xx)
  }
}
