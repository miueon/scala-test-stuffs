package a86

import scala.io.StdIn.*
import scala.collection.mutable.Queue

@main
def test =
  // val (h, w) = readLine.split(" ").map(_.toInt) match
  //   case Array(h, w) => (h, w)

  def readInputs(l: Int): Array[Array[Boolean]] =
    if l == 0 then Array.empty
    else
      val inputs = readLine
        .map(_.toChar)
        .map(it =>
          if it == '#' then true
          else false
        )
        .toArray
      inputs +: readInputs(l - 1)

  def findNextFreeTile(input: Array[Array[Boolean]], i: Int, j: Int, h: Int, w: Int): Option[(Int, Int)] =
    if i == h then None
    else if j == w then findNextFreeTile(input, i + 1, 0, h, w)
    else if !input(i)(j) then Some((i, j))
    else findNextFreeTile(input, i, j + 1, h, w)

  def bfsTurnAllConnectedTileToTrue(input: Array[Array[Boolean]], i: Int, j: Int, h: Int, w: Int): Array[Array[Boolean]] =
    val directions = List((0, 1), (0, -1), (1, 0), (-1, 0))
    def bfs(queue: Queue[(Int, Int)], input: Array[Array[Boolean]]): Array[Array[Boolean]] =
      if queue.isEmpty then input
      else
        while queue.nonEmpty do
          val (i, j) = queue.dequeue()
          directions
            .map { case (di, dj) => (i + di, j + dj) }
            .filter { case (ni, nj) => ni >= 0 && ni < h && nj >= 0 && nj < w && !input(ni)(nj) }
            .filter { case (ni, nj) => ni != i || nj != j }
            .map { case (ni, nj) => (ni, nj) }.foldLeft(queue) { case (acc, it) => acc.enqueue(it) }
            input(i)(j) = true

        input
      // queue match
      //   case Nil => input
      //   case (i, j) :: tail =>
      //     val newQueue = tail ++ directions
      //       .map { case (di, dj) => (i + di, j + dj) }
      //       .filter { case (ni, nj) => ni >= 0 && ni < h && nj >= 0 && nj < w && !input(ni)(nj) }
      //       .filter { case (ni, nj) => ni != i || nj != j }
      //       .map { case (ni, nj) => (ni, nj) }
      //     input(i)(j) = true
      //     bfs(newQueue, input)

    bfs(Queue((i, j)), input)

  def findSplittedZones(input: Array[Array[Boolean]], i: Int, j: Int, h: Int, w: Int, count: Int): Int =
    findNextFreeTile(input, i, j, h, w) match
      case None => count
      case Some(ni, nj) =>
        val newInput = bfsTurnAllConnectedTileToTrue(input, ni, nj, h, w)
        findSplittedZones(newInput, ni, nj, h, w, count + 1)

  def countConnectedRegions(board: Array[Array[Boolean]], h: Int, w: Int): Int = 
    val visited = Array.ofDim[Boolean](h, w)
    def isInsiedBoard(r: Int, c: Int): Boolean = r >= 0 && r < h && c >= 0 && c < w
    def dfs(r: Int, c: Int): Unit =
      if !isInsiedBoard(r, c) || visited(r)(c) || board(r)(c) then return
      else 
        visited(r)(c) = true
        val directions = List((0, 1), (0, -1), (1, 0), (-1, 0))
        for 
          (dr, dc) <- directions
        do
          dfs(r + dr, c + dc)
    var regionCount = 0
    for
      r <- 0 until h; c <- 0 until w if !board(r)(c) && !visited(r)(c)
    do
      dfs(r, c)
      regionCount += 1

    regionCount

  // val input = readInputs(h)
//   .....
// .###.
// .#.#.
// .#.#.
// .#.#.
  val (h, w) = (10, 1)
  val input = Array(
    Array(false),
    Array(true),
    Array(true),
    Array(true),
    Array(false),
    Array(true),
    Array(true),
    Array(true),
    Array(false),
    Array(true)
  )
  // Generate all the different kind of test data to ensure all cases are covered
  val (h1, w1) = (5, 5)
  val input1 = Array(
    Array(true, false, false, false, false),
    Array(false, true, true, true, false),
    Array(false, true, false, true, false),
    Array(false, true, false, true, false),
    Array(false, true, false, true, false)
  )
  val (h2, w2) = (0, 0)
  val input2 = Array.empty[Array[Boolean]]
  val (h3, w3) = (2, 5)
  val input3 = Array(
    Array(true, false, true, false, true),
    Array(false, true, false, true, false)
  )
  val (h5, w5) = (10000, 10000)
// random generate inputs by given h and w
  def randomInput(h: Int, w: Int): Array[Array[Boolean]] = 
    val random = new scala.util.Random
    (0 until h).map { i =>
      (0 until w).map { j =>
        random.nextBoolean()
      }.toArray
    }.toArray

  // val inputs5 = randomInput(h5, w5)

  // println("Test case 1")
  // println(findSplittedZones(inputs5, 0, 0, h5, w5, 0))
  println(countConnectedRegions(input, h, w))
end test


