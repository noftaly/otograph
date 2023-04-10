import java.awt.Color
import scala.swing.Component
import scala.swing.Frame
import scala.swing.Graphics2D
class OrdonnanceGraphComponent(graph: Graph) extends Component {
  private val earliestDates = graph.computeEarliestDates
  private val latestDates = graph.computeLatestDates

  override def paintComponent(g: Graphics2D): Unit = {
    val barHeight = 20
    val barSpacing = 5
    val barWidthPerUnit = 10

    for ((vertex, i) <- graph.vertices.zipWithIndex) {
      val x = earliestDates(vertex) * barWidthPerUnit
      val y = i * (barHeight + barSpacing)
      val width = (latestDates(vertex) - earliestDates(vertex)) * barWidthPerUnit
      val height = barHeight

      g.setColor(Color.BLUE)
      g.fillRect(x, y, width, height)

      g.setColor(Color.BLACK)
      g.drawString(vertex.name, x + width + 5, y + height / 2)
    }
  }
}
