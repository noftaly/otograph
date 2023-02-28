import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Graph(val name: Int):
	private var _vertices: ArrayBuffer[Vertex] = ArrayBuffer()

	def vertices: ArrayBuffer[Vertex] = _vertices
	def vertices_=(vertices: ArrayBuffer[Vertex]): Unit = _vertices = vertices

	def getAlphaVertex: Vertex = _vertices.find(_.isAlpha).get
	def getOmegaVertex: Vertex = _vertices.find(_.isOmega).get

	def hasCycles: Boolean =
		val visited = ArrayBuffer[Vertex]()
		val stack = ArrayBuffer[Vertex]()

		def hasCyclesUtil(vertex: Vertex): Boolean =
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
		for vertex <- _vertices do
			for edge <- vertex.outgoingEdges do
				if edge.duration < 0 then
					return true
		false

	def isSchedulingGraph: Boolean =
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

//def shortestPath: ArrayBuffer[Vertex] =
//	// Dijkstra’s algorithm
//	var cc = ArrayBuffer[Vertex]()
//	var m = _vertices

//	var dijkstraMatrix = Array.ofDim[Int](_vertices.length, _vertices.length)
//

//	while m.nonEmpty do


//	cc

	def computeRanks: scala.collection.mutable.Map[Vertex, Int] =
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

	override def toString: String =
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
		val graph = new Graph(name)

		def findOrCreateVertex(name: String): Vertex =
			graph.vertices.find(_.name == name) match
				case Some(vertex: Vertex) => vertex
				case None =>
					val vertex = new Vertex(name)
					graph.vertices += vertex
					vertex

		for line <- lines do
			val lineSplit = line.split(" ")
			val taskNumber = lineSplit(0)
			val duration = lineSplit(1).toInt
			val predecessors = lineSplit.drop(2).toSet

			val vertex = findOrCreateVertex(taskNumber)

			for predecessorName <- predecessors do
				val predecessor = findOrCreateVertex(predecessorName)

				val edge = new Edge(predecessor, vertex, duration)
				predecessor.addOutgoingEdge(edge)
				vertex.addIncomingEdge(edge)

		val alpha = new Vertex(Vertex.ALPHA_NAME)
		for vertex <- graph.vertices do
			if vertex.incomingEdges.isEmpty then
				val edge = new Edge(alpha, vertex, 0)
				alpha.addOutgoingEdge(edge)
				vertex.addIncomingEdge(edge)

		graph.vertices += alpha

		val omega = new Vertex(Vertex.OMEGA_NAME)
		for vertex <- graph.vertices do
			if vertex.outgoingEdges.isEmpty then
				val edge = new Edge(vertex, omega, 0)
				vertex.addOutgoingEdge(edge)
				omega.addIncomingEdge(edge)

		graph.vertices += omega

		// Sort the vertices so that the first one is the alpha vertex, and the last one is the omega vertex
		graph.vertices = graph.vertices.sortWith(Graph.sorter)

		graph

	def makeFromFile(name: Int): Graph =
		val sourceFile = Source.fromFile(s"./src/resources/$name.txt")

		val lines = sourceFile.getLines.toList
		sourceFile.close()

		makeFromLines(name, lines)

	def printAdjacencyMatrix(graph: Graph): Unit =
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

	def printRanks(graph: Graph): Unit =
		val ranks = graph.computeRanks.toSeq.sortWith((a, b) => Graph.sorter(a._1, b._1))

		for (vertex, rank) <- ranks do
			println(vertex.name + " -> " + rank)
