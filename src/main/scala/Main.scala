import scala.io.Source
import scala.swing._
import scala.swing.event._
import scala.swing.Dialog
import Graph._

object Main extends SimpleSwingApplication {
  var graph: Graph = chooseGraph
  def top: MainFrame = new MainFrame {
    title = "Otograph"
    size = new Dimension(600, 600)
    val commandList = new ListView[String](List(
      "1: Afficher les arêtes",
      "2: Afficher la matrice d'adjacence",
      "3: Afficher les rangs",
      "4: Afficher le tableau de Dijkstra",
      "5: Afficher le calendrier et le chemin critique",
      "6: Afficher le diagramme de Gantt",
      "7: Afficher le graphe",
      "8: Changer de fichier",
      "9: Quitter"
    ))
    val outputArea = new TextArea
    val executeButton = new Button("Exécuter")
    contents = new BoxPanel(Orientation.Vertical) {
      contents += commandList
      contents += executeButton
      contents += outputArea
    }
    listenTo(executeButton)
    reactions += {
      case ButtonClicked(`executeButton`) =>
        val selectedCommand = commandList.selection.items.headOption.map(_.head.toString.toInt)
        selectedCommand match {
          case Some(1) => outputArea.text = displayVertexes(graph)
          case Some(2) => outputArea.text = displayAdjacencyMatrix(graph)
          case Some(3) => outputArea.text = displayRanks(graph)
          case Some(4) => outputArea.text = displayDijkstraMatrix(graph)
          case Some(5) => outputArea.text = displayCalendar(graph)
          case Some(6) => outputArea.text = displayGanttChart(graph)
          case Some(7) => outputArea.text = displayGraph(graph)
          case Some(8) => graph = chooseGraph
          case Some(9) => sys.exit(0)
          case _ => outputArea.text = "Commande invalide"
        }
    }
  }

  private def chooseGraph: Graph = {
    var graph: Option[Graph] = None
    val fileNameField = new TextField
    val fileDialog = new Dialog {
      title = "Choisir un fichier"
      modal = true
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new Label("Entrez un nom de fichier:")
        contents += fileNameField
        contents += new Button(Action("OK") {
          try {
            graph = Some(Graph.makeFromFile(fileNameField.text.toInt))
            dispose()
          } catch {
            case e: Exception => Dialog.showMessage(contents.head, "Nom de fichier invalide", title = "Erreur")
          }
        })
      }
    }
    fileDialog.size = new Dimension(400, 150)
    fileDialog.open()
    graph.get
  }

  private def displayVertexes(graph: Graph): String = {
    val edges = graph.toString
      .split("\n")
      .map(_.split("-->"))
      .map(_.map(_.trim))

    val vertexes = Array.ofDim[Any](edges.length, 3)
    for (i <- edges.indices) {
      if (edges(i).length >= 2) {
        val sourceAndDuration = edges(i)(0).split("---")
        vertexes(i)(0) = sourceAndDuration(0).trim
        vertexes(i)(1) = sourceAndDuration(1).trim
        vertexes(i)(2) = edges(i)(1)
      }
    }

    val table = new Table(vertexes, Array("Source", "Duration", "Destination"))
    // add labels under the table containing additional information
    val labels = new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Is scheduled:" + graph.isSchedulingGraph + "  ")
      contents += new Label("Has cycles:" + graph.hasCycles + "  ")
      contents += new Label("Has negative duration:" + graph.hasNegativeDuration)
    }
    // display all in a scroll pane
    val scrollPane = new ScrollPane(table)
    val boxPanel = new BoxPanel(Orientation.Vertical) {
      contents += scrollPane
      contents += labels
    }
    val frame = new Frame {
      title = "Vertexes"
      contents = boxPanel
      visible = true
    }

    "Done displaying vertexes"
  }

  def displayAdjacencyMatrix(graph: Graph): String = {
    val matrix = graph.adjacencyMatrix
    val data = Array.ofDim[Any](matrix.length, matrix.length + 1)

    // Add column headers
    // Set "headers" to an array where the first element is empty, and the rest are the column headers
    val headers = Array.ofDim[String](matrix.length + 1)
    headers(0) = ""
    for (i <- graph.vertices.indices) do
      headers(i + 1) = graph.vertices(i).name

    // Add row headers and data
    for (i <- matrix.indices) {
      data(i)(0) = graph.vertices(i).name
      for (j <- matrix.indices) {
        data(i)(j + 1) = matrix(i)(j) match {
          case Some(value) => value.toString
          case None => "•"
        }
      }
    }

    val table = new Table(data, headers)
    val scrollPane = new ScrollPane(table)
    val frame = new Frame {
      title = "Adjacency Matrix"
      contents = scrollPane
      visible = true
    }

    "Done displaying adjacency matrix"
  }

  def displayRanks(graph: Graph): String = {
    if (graph.hasCycles || graph.hasNegativeDuration) {
      Dialog.showMessage(null, "We cannot display the ranks for this graph.")
      return "Failed displaying ranks"
    }

    val ranks = graph.computeRanks.toSeq.sortWith((a, b) => Graph.sorter(a._1, b._1))
    val data = Array.ofDim[Any](ranks.length, 2)

    // Add data
    for (i <- ranks.indices) {
      val (vertex, rank) = ranks(i)
      data(i)(0) = vertex.name
      data(i)(1) = rank.toString
    }

    val table = new Table(data, Array("Vertex", "Rank"))
    val scrollPane = new ScrollPane(table)
    val frame = new Frame {
      title = "Ranks"
      contents = scrollPane
      visible = true
    }

    "Done displaying ranks"
  }

  def displayDijkstraMatrix(graph: Graph): String = {
    val matrix = graph.dijkstraMatrix
    val data = Array.ofDim[Any](matrix.length, matrix.length + 1)

    // Add column headers
    // Set "headers" to an array where the first element is empty, and the rest are the column headers
    val headers = Array.ofDim[String](matrix.length + 1)
    headers(0) = ""
    for (i <- graph.vertices.indices) do
      headers(i + 1) = graph.vertices(i).name

    // Add row headers and data
    for (i <- matrix.indices) {
      data(i)(0) = graph.vertices(i).name
      for (j <- matrix.indices) {
        data(i)(j + 1) = matrix(i)(j) match {
          case null => "•"
          case i: Int => if (i == Int.MaxValue) "∞" else i.toString
        }
      }
    }

    val table = new Table(data, headers)
    val scrollPane = new ScrollPane(table)
    val frame = new Frame {
      title = "Dijkstra Matrix"
      contents = scrollPane
      visible = true
    }

    "Done displaying Dijkstra matrix"
  }

  def displayCalendar(graph: Graph): String = {
    if (graph.hasCycles || graph.hasNegativeDuration) {
      Dialog.showMessage(null,"We cannot display the calendar for this graph.")
      "Failed displaying calendar"
    } else {
      val earliestDates = graph.computeEarliestDates
      val latestDates = graph.computeLatestDates
      val slacks = graph.computeSlacks
      val ranks = graph.computeRanks
      val criticalPaths = graph.computeCriticalPaths

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

      val data = Array.ofDim[Any](sortedVertices.length + criticalPaths.length + 1, 5)

      // Add data for each vertex
      for (i <- sortedVertices.indices) {
        val vertex = sortedVertices(i)
        val earliestDate = earliestDates(vertex)
        val latestDate = latestDates(vertex)
        val slack = slacks(vertex)
        val rank = ranks(vertex)

        data(i)(0) = vertex.name
        data(i)(1) = rank.toString
        data(i)(2) = earliestDate.toString
        data(i)(3) = latestDate.toString
        data(i)(4) = slack.toString
      }

      // Add critical path information
      if (criticalPaths.isEmpty) {
        data(sortedVertices.length)(0) = "The graph has cycles or negative duration and does not have a critical path."
      } else {
        data(sortedVertices.length)(0) = "Criticals paths list is: "
        for (i <- criticalPaths.indices) {
          data(sortedVertices.length + i + 1)(0) = criticalPaths(i).map(_.name).mkString(" -> ")
        }
      }

      val table = new Table(data, Array("Vertices", "Rank", "Earliest Date", "Latest Date", "Slack"))
      val scrollPane = new ScrollPane(table)
      val frame = new Frame {
        title = "Calendar"
        contents = scrollPane
        visible = true
      }

      "Done displaying calendar and critical path"
    }

  }

  def displayGanttChart(graph: Graph): String = {
    val ordonnanceGraphComponent = new OrdonnanceGraphComponent(graph)
    val frame = new Frame {
      title = "Ordonnance Graph"
      contents = ordonnanceGraphComponent
      size = new Dimension(500, 500)
      visible = true
    }
    "Done displaying Gantt chart"
  }

  def displayGraph(graph: Graph): String = {
    val graphComponent = new GraphComponent(graph)
    val frame = new Frame {
      title = "Graph"
      contents = graphComponent
      visible = true
      size = new Dimension(500, 500)
    }
    "Done displaying graph"
  }
}
