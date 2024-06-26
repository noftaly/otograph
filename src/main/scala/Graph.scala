import scala.collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer, Map, Queue}
import scala.io.Source
import javax.swing.*
import scala.collection.mutable

class Graph(val name: Int):
	private var _vertices: ArrayBuffer[Vertex] = ArrayBuffer()

	def vertices: ArrayBuffer[Vertex] = _vertices
	def vertices_=(vertices: ArrayBuffer[Vertex]): Unit = _vertices = vertices

	private def getAlphaVertex: Vertex = _vertices.find(_.isAlpha).get
	private def getOmegaVertex: Vertex = _vertices.find(_.isOmega).get

	def hasCycles: Boolean =
		val visited = ArrayBuffer[Vertex]()
		val stack = ArrayBuffer[Vertex]()

		def hasCyclesUtil(vertex: Vertex): Boolean =
			// If the vertex is already in the stack, then there is a cycle
			if !visited.contains(vertex) then
				visited += vertex
				stack += vertex

				for edge <- vertex.outgoingEdges do
					if (!visited.contains(edge.to) && hasCyclesUtil(edge.to)) || stack.contains(edge.to) then
						return true

			stack -= vertex
			false

		hasCyclesUtil(_vertices.head)

	def hasNegativeDuration: Boolean =
		// If there is a negative edge, then the graph is not a scheduling graph
		for vertex <- _vertices do
			for edge <- vertex.outgoingEdges do
				if edge.duration < 0 then
					return true
		false

	def isSchedulingGraph: Boolean =
		// A scheduling graph must satisfy the following conditions:
		// 1. A single entry point
		if _vertices.count(_.isAlpha) != 1 then return false
		// 2. A single exit point
		if _vertices.count(_.isOmega) != 1 then return false
		// 3. No cycles
		if hasCycles then return false
		// 4. Same weights for all outgoing edges of a vertex
		if _vertices.exists(_.outgoingEdges.map(_.duration).distinct.size > 1) then return false
		// 5. Outgoing edges of the entry vertex have zero
		if getAlphaVertex.outgoingEdges.exists(_.duration != 0) then return false
		// 6. No negative edges
		if hasNegativeDuration then return false

		true

	def adjacencyMatrix: Array[Array[Option[Int]]] =

		// Create a matrix of size |V| x |V| and fill it with None
		val matrix = Array.ofDim[Option[Int]](_vertices.size, _vertices.size)

		for i <- matrix.indices do
			for j <- matrix.indices do
				matrix(i)(j) = None

		for vertex <- _vertices do
			for edge <- vertex.outgoingEdges do
				val predecessorIndex = _vertices.indexOf(edge.from)
				val successorIndex = _vertices.indexOf(edge.to)
				matrix(predecessorIndex)(successorIndex) = Some(edge.duration)
		matrix

	def dijkstraMatrix: ArrayBuffer[ArrayBuffer[Int|Null]] =
		// Dijkstra’s algorithm
		val cc = ArrayBuffer[Vertex]()
		val m =	_vertices.clone()
		var actualVertex = _vertices.head
		val dijkstraMatrix: ArrayBuffer[ArrayBuffer[Int|Null]] = ArrayBuffer.fill(_vertices.size, _vertices.size)(Int.MaxValue)
		var i = 0
		val edgeMap = _vertices.map(_.name).zipWithIndex.toMap
		dijkstraMatrix(i)(edgeMap(actualVertex.name)) = 0

		while m.nonEmpty do
			cc += actualVertex
			m -= actualVertex

			if i > 0 then
				for n <- _vertices.indices do
					// If the value is not null, then it is the value of the previous iteration
					if dijkstraMatrix(i)(n) != null then
						dijkstraMatrix(i)(n) = dijkstraMatrix(i-1)(n)

			for edge <- actualVertex.outgoingEdges do
				// If the vertex is not in the closed set
				if m.contains(edge.to) then
					val actualValue = edge.duration + dijkstraMatrix(i)(edgeMap(actualVertex.name)).asInstanceOf[Int]
					val oldValue = if i == 0 then Int.MaxValue else dijkstraMatrix(i - 1)(edgeMap(edge.to.name)).asInstanceOf[Int]
					dijkstraMatrix(i)(edgeMap(edge.to.name)) = Math.min(actualValue, oldValue)

			for n <- i+1 until _vertices.size do
				dijkstraMatrix(n)(edgeMap(actualVertex.name)) = null

			if m.nonEmpty then
				// Find the vertex with the minimum value in the last row
				val minEdge = m.minBy((v: Vertex) => dijkstraMatrix.map(row =>
					row(edgeMap(v.name)) match
						case null => Int.MaxValue
						case value => value.asInstanceOf[Int]
				).min)

				actualVertex = minEdge

			i += 1

		dijkstraMatrix

	def computeRanks: scala.collection.mutable.Map[Vertex, Int] =
		// Kahn's algorithm
		val ranks = scala.collection.mutable.Map[Vertex, Int]()
		val predecessors = Array.ofDim[Int](_vertices.length)

		// For all vertices, set their rank to the number of incoming edges
		for (vertex, idx) <- _vertices.zipWithIndex do
			predecessors(idx) = vertex.incomingEdges.length

		var k = 0
		var sources = Array[Set[Vertex]]()

		sources :+= Set[Vertex]()
		sources(0) += getAlphaVertex

		while sources.length != _vertices.length do
			sources :+= Set[Vertex]()

			for vertex <- sources(k) do
				ranks += (vertex -> k)

				for successorVertex <- vertex.outgoingEdges.map(_.to) do
					predecessors(_vertices.indexOf(successorVertex)) -= 1

					if predecessors(_vertices.indexOf(successorVertex)) == 0 then
						sources(k + 1) += successorVertex
			k += 1
		ranks
	// adjacent function take two vertices and return true if they are adjacent
	def adjacent(v1: Vertex, v2: Vertex): Boolean = { // alternate function name for graph displaying
		v1.outgoingEdges.exists(_.to == v2)
	}
	def getEdge(vertex1: Vertex, vertex2: Vertex): Edge = {
		vertex1.outgoingEdges.find(_.to == vertex2).get
	}
	def computeEarliestDates: Map[Vertex, Int] =
		// Kahn's algorithm
		val ranks = computeRanks
		val earliestDates = Map[Vertex, Int]()

		// For all vertices, set their earliest date to 0
		for vertex <- _vertices do
			earliestDates += (vertex -> 0)

		var k = 0
		var sources = Array[Set[Vertex]]()

		sources :+= Set[Vertex]()
		sources(0) += getAlphaVertex

		while sources.length != _vertices.length do
			sources :+= Set[Vertex]()

			for vertex <- sources(k) do
				for successorVertex <- vertex.outgoingEdges.map(_.to) do
					val newEarliestDate = earliestDates(vertex) + vertex.outgoingEdges.find(_.to == successorVertex).get.duration
					if newEarliestDate > earliestDates(successorVertex) then
						earliestDates += (successorVertex -> newEarliestDate)

					if ranks(successorVertex) == k + 1 then
						sources(k + 1) += successorVertex
			k += 1
		earliestDates

	def computeLatestDates: scala.collection.mutable.Map[Vertex, Int] = {
		// Kahn's algorithm
		val ranks = computeRanks
		val latestDates = scala.collection.mutable.Map[Vertex, Int]()

		// Set the latest date of the omega vertex as the current date
		// latestDates(getOmegaVertex) = 0
		latestDates(getOmegaVertex) = computeEarliestDates(getOmegaVertex)

		// For each rank starting from the omega vertex, find the latest date of the predecessors
		for (k <- (ranks(getOmegaVertex) - 1) to 0 by -1) {
			for (vertex <- _vertices if ranks(vertex) == k) {
				// Compute the latest date of the vertex as the minimum of the latest dates of its successors
				val latestDate = vertex.outgoingEdges.map(edge => latestDates(edge.to) - edge.duration).min
				// Update the latest date of the vertex
				latestDates(vertex) = latestDate
			}
		}

		latestDates
	}

	//Slacks (float) = latest date - earliest date
	def computeSlacks: scala.collection.mutable.Map[Vertex, Int] =
		// Substract the latest date to the earliest date
		val slacks = scala.collection.mutable.Map[Vertex, Int]()

		// For all vertices, set their slack (float) to the difference between the latest date and the earliest date
		for vertex <- _vertices do
			slacks += (vertex -> (computeLatestDates(vertex) - computeEarliestDates(vertex)))
		slacks

	def computeCriticalPaths: ArrayBuffer[ArrayBuffer[Vertex]] =
		// Compute critical slacks
		val earlyDates = computeEarliestDates

		val criticalSlacks = computeSlacks
			.filter(_._2 == 0).keys.toArray
			.filter(v => v.name != Vertex.ALPHA_NAME)
			.sortWith((a, b) => earlyDates(a) < earlyDates(b))

		// Do a recursive function to find a path with a list of vertex
		def pathFinder(currentPath: ArrayBuffer[Vertex], paths: ArrayBuffer[ArrayBuffer[Vertex]]): Unit =
			if currentPath.last.isOmega then
				// If the current vertex is the omega vertex, add the path to the list of critical paths
				paths += currentPath
			else
				// If the current vertex is not the omega vertex, find the next vertex
				// Find the next vertices
				val nextVertices = currentPath.last.outgoingEdges
					.map(_.to)
					.filter(vertex => criticalSlacks.contains(vertex))

				// For each of the possible successors, create a new "branch" (a new path the same as the one that
				// landed you here), and add the successor to the end of this new path.
				for nextVertex <- nextVertices do
					val newCurrentPath = currentPath.clone()
					newCurrentPath += nextVertex
					pathFinder(newCurrentPath, paths)


		var paths = ArrayBuffer[ArrayBuffer[Vertex]]()
		pathFinder(ArrayBuffer(getAlphaVertex), paths)

		def computeDurationPath(path: ArrayBuffer[Vertex]): Int =
			var duration = 0
			for i <- 0 until path.length - 1 do
				duration += getEdge(path(i), path(i + 1)).duration
			duration

		val goalDuration = earlyDates.find(_._1.isOmega).get._2

		paths.filter(path => computeDurationPath(path) == goalDuration)

	override def toString: String =
		// Sort the vertices by name
		var res = ""
		for vertex <- _vertices do
			for edge <- vertex.outgoingEdges do
				res += vertex.name + "  ---" + edge.duration + "-->  " + edge.to.name + "\n"

		res += "\n\n- Is scheduling graph: " + isSchedulingGraph + "\n"
		res += "    ↳ Has cycles: " + hasCycles + "\n"
		res += "    ↳ Has negative durations: " + hasNegativeDuration + "\n"

		res
object Graph:
	def sorter(a: Vertex, b: Vertex): Boolean =
		if a.name == Vertex.OMEGA_NAME || b.name == Vertex.ALPHA_NAME then
			return false
		if a.name == Vertex.ALPHA_NAME || b.name == Vertex.OMEGA_NAME then
			return true
		a.name.toInt < b.name.toInt

	def makeFromLines(name: Int, lines: List[String]): Graph =
		// Create the graph
		val graph = new Graph(name)

		def makeEdge(from: Vertex, to: Vertex): Unit =
			val edge = new Edge(from, to, from.duration)
			from.addOutgoingEdge(edge)
			to.addIncomingEdge(edge)

		// First pass over all the lines to create the vertices
		for line <- lines do
			val lineSplit = line.split(" ")
			val taskNumber = lineSplit(0)
			val duration = lineSplit(1).toInt

			val vertex = new Vertex(taskNumber, duration)
			graph.vertices += vertex

		// Second pass over all the lines to create the edges
		for line <- lines do
			val lineSplit = line.split(" ")
			val taskNumber = lineSplit(0)
			val predecessors = lineSplit.drop(2).toSet

			val vertex = graph.vertices.find(_.name == taskNumber).get

			for predecessorName <- predecessors do
				val predecessor = graph.vertices.find(_.name == predecessorName).get
				makeEdge(predecessor, vertex)

		val alpha = new Vertex(Vertex.ALPHA_NAME, 0)
		for vertex <- graph.vertices do
			if vertex.incomingEdges.isEmpty then
				makeEdge(alpha, vertex)

		graph.vertices += alpha

		val omega = new Vertex(Vertex.OMEGA_NAME, 0)
		for vertex <- graph.vertices do
			if vertex.outgoingEdges.isEmpty then
				makeEdge(vertex, omega)

		graph.vertices += omega

		// Sort the vertices so that the first one is the alpha vertex, and the last one is the omega vertex
		graph.vertices = graph.vertices.sortWith(Graph.sorter)

		graph

	def makeFromFile(name: Int): Graph =
		// Read the file
		val sourceFile = Source.fromFile(s"./src/resources/$name.txt")

		val lines = sourceFile.getLines.toList
		sourceFile.close()

		makeFromLines(name, lines)

	def printAdjacencyMatrix(graph: Graph): Unit = {
		// Get the adjacency matrix
		val matrix = graph.adjacencyMatrix

		// Print all the vertices names (padded to 2 chars with a leading 0), and separated by a space
		val names = graph.vertices.map(_.name).map(name => name match
			case Vertex.ALPHA_NAME | Vertex.OMEGA_NAME => s" $name"
			case _ => f"${name.toInt}%02d"
		)

		println("   " + names.mkString(" "))
		for i <- matrix.indices do
			print(names(i) + " ")
			for j <- matrix.indices do
				print(matrix(i)(j) match
					case Some(value) => f"$value% 2d "
					case None => " • "
				)
			println()
	}

	def toStringAdjacencyMatrix(graph: Graph): String = {
		// Print the adjacency matrix
		val matrix = graph.adjacencyMatrix
		val result = new StringBuilder

		// Print all the vertices names (padded to 2 chars with a leading 0), and separated by a space
		val names = graph.vertices.map(_.name).map(name => name match {
			case Vertex.ALPHA_NAME | Vertex.OMEGA_NAME => s" $name"
			case _ => f"${name.toInt}%02d"
		})

		result.append("   " + names.mkString(" ") + "\n")
		for (i <- matrix.indices) {
			result.append(names(i) + " ")
			for (j <- matrix.indices) {
				result.append(matrix(i)(j) match {
					case Some(value) => f"$value% 2d "
					case None => " • "
				})
			}
			result.append("\n")
		}
		result.toString
	}
	def printRanks(graph: Graph): Unit = {
		// Print the ranks
		val ranks = graph.computeRanks.toSeq.sortWith((a, b) => Graph.sorter(a._1, b._1))

		for (vertex, rank) <- ranks do
			println(vertex.name + " -> " + rank)
	}
	def toStringRanks(graph: Graph): String = {
			val ranks = graph.computeRanks.toSeq.sortWith((a, b) => Graph.sorter(a._1, b._1))
			val result = new StringBuilder
			for ((vertex, rank) <- ranks) {
				result.append(vertex.name + " -> " + rank + "\n")
			}
			result.toString
		}
	def printDijkstraMatrix(graph:Graph): Unit = {
		// Print the Dijkstra matrix
		val matrix = graph.dijkstraMatrix

		// Print all the vertices names (padded to 2 chars with a leading 0), and separated by a space
		val names = graph.vertices.map(_.name).map(name => name match
			case Vertex.ALPHA_NAME | Vertex.OMEGA_NAME => s" $name"
			case _ => f"${name.toInt}%02d"
		)

		println(names.mkString(" "))
		for i <- matrix.indices do
			for j <- matrix.indices do
				print(matrix(i)(j) match
					case null => " • "
					case i: Int => if i == Int.MaxValue then " ∞ " else f"$i% 2d "
				)
			println()
	}

	def toStringDijkstraMatrix(graph: Graph): String = {
		// Print the Dijkstra matrix
		val matrix = graph.dijkstraMatrix
		val result = new StringBuilder

		// Print all the vertices names (padded to 2 chars with a leading 0), and separated by a space
		val names = graph.vertices.map(_.name).map(name => name match {
			case Vertex.ALPHA_NAME | Vertex.OMEGA_NAME => s" $name"
			case _ => f"${name.toInt}%02d"
		})

		result.append(names.mkString(" ") + "\n")
		for (i <- matrix.indices) {
			for (j <- matrix.indices) {
				result.append(matrix(i)(j) match {
					case null => " • "
					case i: Int => if (i == Int.MaxValue) " ∞ " else f"$i% 2d "
				})
			}
			result.append("\n")
		}
		result.toString
	}
	def printCalendar(graph: Graph): Unit = {
		// Print the calendar
		if (graph.hasCycles || graph.hasNegativeDuration) {
			println("We cannot display the calendar for this graph.")
		} else {
			val earliestDates = graph.computeEarliestDates
			val latestDates = graph.computeLatestDates
			val slacks = graph.computeSlacks
			val ranks = graph.computeRanks

			// Sort vertices by rank, then by name or number
			val sortedVertices = graph.vertices.toSeq.sortBy { vertex =>
				if (vertex.name == "ɑ") {
					(0, -1) // ɑ comes first
				} else if (vertex.name == "ω") {
					(Int.MaxValue, Int.MaxValue) // ω comes last
				} else {
					(ranks(vertex), vertex.name.toInt) // sort by rank, then by number
				}
			}

			// Print the header
			println(f"${"Vertices"}%8s | ${"Rank"}%4s | ${"Earliest Date"}%13s | ${"Latest Date"}%11s | ${"Slack"}%5s")
			println("=" * 53)

			// Print the data for each vertex
			for (vertex <- sortedVertices) {
			val earliestDate = earliestDates(vertex)
			val latestDate = latestDates(vertex)
			val slack = slacks(vertex)
			val rank = ranks(vertex)

			println(f"${vertex.name}%8s | ${rank}%4d | ${earliestDate}%13d | ${latestDate}%11d | ${slack}%5d")
			}

			print("\n")

			if (graph.hasCycles || graph.hasNegativeDuration) {
				println("The graph has cycles or negative duration and does not have a critical path.")
			} else {
				val criticalPaths = graph.computeCriticalPaths
				println("The critical path is:")
				for (path <- criticalPaths) {
					println(path.map(_.name).mkString(" -> "))
				}
			}
		}
	}

	def toStringCalendar(graph: Graph): String = {
		val result = new StringBuilder
		if (graph.hasCycles || graph.hasNegativeDuration) {
			result.append("We cannot display the calendar for this graph.\n")
		} else {
			val earliestDates = graph.computeEarliestDates
			val latestDates = graph.computeLatestDates
			val slacks = graph.computeSlacks
			val ranks = graph.computeRanks

			// Sort vertices by rank, then by name or number
			val sortedVertices = graph.vertices.toSeq.sortBy { vertex =>
				if (vertex.name == "ɑ") {
					(0, -1) // ɑ comes first
				} else if (vertex.name == "ω") {
					(Int.MaxValue, Int.MaxValue) // ω comes last
				} else {
					(ranks(vertex), vertex.name.toInt) // sort by rank, then by number
				}
			}

			// Print the header
			result.append(f"${"Vertices"}%8s | ${"Rank"}%4s | ${"Earliest Date"}%13s | ${"Latest Date"}%11s | ${"Slack"}%5s\n")
			result.append("=" * 53 + "\n")

			// Print the data for each vertex
			for (vertex <- sortedVertices) {
				val earliestDate = earliestDates(vertex)
				val latestDate = latestDates(vertex)
				val slack = slacks(vertex)
				val rank = ranks(vertex)

				result.append(f"${vertex.name}%8s | ${rank}%4d | ${earliestDate}%13d | ${latestDate}%11d | ${slack}%5d\n")
			}

			result.append("\n")
			val criticalPath = graph.computeCriticalPaths
			if (criticalPath.isEmpty) {
				result.append("The graph has cycles or negative duration and does not have a critical path.\n")
			} else {
				result.append("The critical path is:\n")
				//result.append(criticalPath.map(_.name).mkString(" -> ") + "\n")
			}
		}
		result.toString
	}
