import scala.io.Source
import scala.io.StdIn.readInt

@main def main: Unit =
	var quit = false
	var graph = chooseGraph

	println(graph.computeRanks)
	while !quit do
		println("""Enter a command:
			|1: Print the edges
			|2: Print the adjacency matrix
			|3: Change the graph
			|4: Quit""".stripMargin)
		try
			val command = readInt()
			command match
				case 1 => println(graph)
				case 2 => graph.printAdjacencyMatrix()
				case 3 => graph = chooseGraph
				case 4 => quit = true
				case _ => println("Invalid command")
		catch
			case e: Exception => println("Invalid command")

def chooseGraph: Graph =
	var graph: Option[Graph] = None
	while graph.isEmpty do
		println("Enter a file name:")
		try
			val fileName = readInt()
			graph = Some(Graph.makeFromFile(fileName))
		catch
			case e: Exception => println("Invalid file name")
	graph.get
