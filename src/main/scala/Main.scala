import scala.io.Source
import scala.swing._
import scala.swing.event._
import Graph._
object Main extends SimpleSwingApplication {
  var graph = chooseGraph

  def top = new MainFrame {
    title = "Graph GUI"
    val commandList = new ListView[String](List(
      "1: Afficher les arêtes",
      "2: Afficher la matrice d'adjacence",
      "3: Afficher les rangs",
      "4: Afficher le tableau de Dijkstra",
      "5: Afficher le calendrier et le chemin critique",
      "6: Changer le graphique",
      "7: Quitter"
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
          case Some(1) => outputArea.text = graph.toString
          case Some(2) => outputArea.text = Graph.toStringAdjacencyMatrix(graph)
          case Some(3) => outputArea.text = Graph.toStringRanks(graph)
          case Some(4) => outputArea.text = Graph.toStringDijkstraMatrix(graph)
          case Some(5) => outputArea.text = Graph.toStringCalendar(graph)
          case Some(6) => graph = chooseGraph
          case Some(7) => sys.exit(0)
          case _ => outputArea.text = "Commande invalide"
        }
    }
  }

  def chooseGraph: Graph = {
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
            val fileName = fileNameField.text
            graph = Some(Graph.makeFromFile(fileName.toInt))
            dispose()
          } catch {
            case e: Exception => Dialog.showMessage(contents.head, "Nom de fichier invalide", title="Erreur")
          }
        })
      }
    }
    fileDialog.open()
    graph.get
  }
}