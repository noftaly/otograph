import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class Graph(val name: String):
	private var _vertices: ArrayBuffer[Vertex] = ArrayBuffer()

	def vertices: ArrayBuffer[Vertex] = _vertices

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
		!hasCycles && !hasNegativeDuration

	def getFromFile: Graph =
		val sourceFile = Source.fromFile(s"./src/resources/$name.txt")
		val lines = sourceFile.getLines.toList
		sourceFile.close()

		def findOrCreateVertex(name: String): Vertex =
			_vertices.find(_.name == name) match
				case Some(vertex: Vertex) => vertex
				case None =>
					val vertex = new Vertex(name)
					_vertices += vertex
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
		for vertex <- _vertices do
			if vertex.incomingEdges.isEmpty then
				val edge = new Edge(alpha, vertex, 0)
				alpha.addOutgoingEdge(edge)
				vertex.addIncomingEdge(edge)

		_vertices += alpha

		val omega = new Vertex(Vertex.OMEGA_NAME)
		for vertex <- _vertices do
			if vertex.outgoingEdges.isEmpty then
				val edge = new Edge(vertex, omega, 0)
				vertex.addOutgoingEdge(edge)
				omega.addIncomingEdge(edge)

		_vertices += omega

		def sorter(a: Vertex, b: Vertex): Boolean =
			if a.name == Vertex.OMEGA_NAME || b.name == Vertex.ALPHA_NAME  then
				return false
			if a.name == Vertex.ALPHA_NAME || b.name == Vertex.OMEGA_NAME  then
				return true
			a.name.toInt < b.name.toInt

		// Sort the vertices so that the first one is the alpha vertex, and the last one is the omega vertex
		_vertices  = _vertices.sortWith(sorter)

		this

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

	def printAdjacencyMatrix(): Unit =
		val matrix = adjacencyMatrix

		// Print all the vertices names (padded to 2 chars with a leading 0), and separated by a space
		val names = _vertices.map(_.name).map(name => name match
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

	override def toString: String =
		var res = ""
		for vertex <- _vertices do
			for edge <- vertex.outgoingEdges do
				res += vertex.name + " ---" + edge.duration + "--> " + edge.to.name + "\n"

		res += "\n\n- Has cycles: " + hasCycles + "\n"
		res += "- Has negative durations: " + hasNegativeDuration + "\n"
		res += "- Is scheduling graph: " + isSchedulingGraph + "\n"
		res
