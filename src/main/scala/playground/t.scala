package playPure

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

type Graph = Map[Int, List[(Int, Int)]] // Adjacency list: Map[node, List[(neighbor, weight)]]

def dijkstra(graph: Graph, start: Int): Map[Int, Int] =
    // Ordering for the priority queue (smallest distance first)
    given Ordering[(Int, Int)] = Ordering.by[(Int, Int), Int](_._2)

    @tailrec
    def dijkstraHelper(
        distances: Map[Int, Int], // Current shortest distances
        queue: TreeSet[(Int, Int)] // Priority queue of (node, distance)
    ): Map[Int, Int] =
        if queue.isEmpty then
            distances // Base case: no more nodes to process
        else
            val ((current, currentDist), restQueue) = (queue.head, queue.tail)
            // Skip if we've already found a shorter path to this node
            if currentDist > distances(current) then
                dijkstraHelper(distances, restQueue)
            else
                val neighbors = graph.getOrElse(current, List())
                val (updatedDistances, updatedQueue) = neighbors.foldLeft((distances, restQueue)) {
                    case ((accDistances, accQueue), (neighbor, weight)) =>
                        val newDistance = currentDist + weight
                        if newDistance < accDistances.getOrElse(neighbor, Int.MaxValue) then
                            val newDistances = accDistances + (neighbor -> newDistance)
                            val newQueue = accQueue + ((neighbor, newDistance))
                            (newDistances, newQueue)
                        else
                            (accDistances, accQueue)
                }
                dijkstraHelper(updatedDistances, updatedQueue)

    // Initialize distances: start node has distance 0, others have infinity (Int.MaxValue)
    val initialDistances = graph.keys.map(_ -> Int.MaxValue).toMap + (start -> 0)
    // Initialize priority queue with the start node
    val initialQueue = TreeSet((start, 0))
    dijkstraHelper(initialDistances, initialQueue)

// Example usage
val graph: Graph = Map(
    1 -> List((2, 7), (3, 9), (6, 14)),
    2 -> List((1, 7), (3, 10), (4, 15)),
    3 -> List((1, 9), (2, 10), (4, 11), (6, 2)),
    4 -> List((2, 15), (3, 11), (5, 6)),
    5 -> List((4, 6), (6, 9)),
    6 -> List((1, 14), (3, 2), (5, 9))
)

@main
def test =
  val shortestPaths = dijkstra(graph, 1)

  println(shortestPaths)
