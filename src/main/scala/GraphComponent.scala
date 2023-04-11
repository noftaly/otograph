import java.awt.Color
import scala.swing.Component
import scala.swing.Graphics2D
import Graph._

class GraphComponent(graph: Graph) extends Component {
  private val nodeSize = 30
  private val nodeSpacing = 50

  override def paintComponent(g: Graphics2D): Unit = {
    val n = graph.vertices.size
    val angleSpacing = 2 * math.Pi / n
    val radius = math.min(size.width, size.height) / 2 - nodeSize

    for ((vertex1, i) <- graph.vertices.zipWithIndex) {
      val angle = i * angleSpacing
      val x1 = (size.width / 2 + radius * math.cos(angle) - nodeSize / 2).toInt
      val y1 = (size.height / 2 + radius * math.sin(angle) - nodeSize / 2).toInt

      // Draw node
      g.setColor(Color.WHITE)
      g.fillOval(x1, y1, nodeSize, nodeSize)
      g.setColor(Color.BLACK)
      g.drawOval(x1, y1, nodeSize, nodeSize)
      g.drawString(vertex1.name, x1 + nodeSize / 2, y1 + nodeSize / 2)

      // Draw edges
      for (vertex2 <- graph.vertices) {
        if (graph.adjacent(vertex1, vertex2)) {
          val j = graph.vertices.indexOf(vertex2)
          val angle2 = j * angleSpacing
          val x2 = (size.width / 2 + radius * math.cos(angle2) - nodeSize / 2).toInt
          val y2 = (size.height / 2 + radius * math.sin(angle2) - nodeSize / 2).toInt
          g.drawLine(x1 + nodeSize / 2, y1 + nodeSize / 2, x2 + nodeSize / 2, y2 + nodeSize / 2)

          // Draw edge label
          val edgeMidX = (x1 + x2) / 2
          val edgeMidY = (y1 + y2) / 2
          g.drawString(graph.getEdge(vertex1, vertex2).duration.toString, edgeMidX, edgeMidY)

          // Draw arrowhead
          val arrowLength = 10
          val arrowWidth = 7

          val dx = x2 - x1
          val dy = y2 - y1
          val theta = math.atan2(dy, dx)
          val phi = math.atan(arrowWidth.toDouble / arrowLength)

          // Adjust starting point of arrow
          val arrowStartX = x2 - nodeSize / 2 * math.cos(theta)
          val arrowStartY = y2 - nodeSize / 2 * math.sin(theta)

          val xPoints = new Array[Int](3)
          val yPoints = new Array[Int](3)

          xPoints(0) = arrowStartX.toInt
          yPoints(0) = arrowStartY.toInt

          xPoints(1) = (arrowStartX - arrowLength * math.cos(theta + phi)).toInt
          yPoints(1) = (arrowStartY - arrowLength * math.sin(theta + phi)).toInt

          xPoints(2) = (arrowStartX - arrowLength * math.cos(theta - phi)).toInt
          yPoints(2) = (arrowStartY - arrowLength * math.sin(theta - phi)).toInt

          g.fillPolygon(xPoints, yPoints, 3)
        }
      }
    }
  }
}