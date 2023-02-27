import scala.io.Source

@main def main: Unit =
	val graph = new Graph("table 1").getFromFile
	print(graph)
	println("--------------")
	graph.printAdjacencyMatrix()

