import scala.io.Source
import scala.io.StdIn.readInt

@main def main: Unit =
	var quit = false
	var graph = chooseGraph

	while !quit do
		println("\n\n\n\n")
		println("""Enter a command:
			|1: Print the edges
			|2: Print the adjacency matrix
			|3: Print the ranks
			|4: Change the graph
			|5: Quit""".stripMargin)
		try
			val command = readInt()
			println("\n\n\n\n")
			command match
				case 1 => println(graph)
				case 2 => Graph.printAdjacencyMatrix(graph)
				case 3 => Graph.printRanks(graph)
				case 4 => graph = chooseGraph
				case 5 => quit = true
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
