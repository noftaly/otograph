import scala.collection.mutable.ArrayBuffer

class Vertex(val name: String):
	private val _outgoingEdges: ArrayBuffer[Edge] = ArrayBuffer()
	private val _incomingEdges: ArrayBuffer[Edge] = ArrayBuffer()

	def outgoingEdges: ArrayBuffer[Edge] = _outgoingEdges
	def incomingEdges: ArrayBuffer[Edge] = _incomingEdges

	def addOutgoingEdge(edge: Edge): Unit = _outgoingEdges += edge
	def addIncomingEdge(edge: Edge): Unit = _incomingEdges += edge

	def isAlpha: Boolean = name == Vertex.ALPHA_NAME
	def isOmega: Boolean = name == Vertex.OMEGA_NAME

object Vertex:
	val ALPHA_NAME = "ɑ"
	val OMEGA_NAME = "ω"
