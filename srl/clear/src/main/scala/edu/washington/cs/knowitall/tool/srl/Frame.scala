package edu.washington.cs.knowitall.tool.srl

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.tokenize.Token
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

case class Frame(relation: Relation, arguments: Seq[Argument]) {
  override def toString = relation.toString + ":" + arguments.mkString("[", ", ", "]")
  def serialize = relation.serialize + ":" + arguments.map(_.serialize).mkString("[", ", ", "]")
}
object Frame {
  val pickledRegex = """([^:]*):\[(.*)\]""".r
  def deserialize(dgraph: DependencyGraph)(pickled: String) = {
    pickled match {
      case pickledRegex(relation, arguments) =>
        val rel = Relation.deserialize(dgraph)(relation)
        val args = arguments.split(",\\s+") map Argument.deserialize(dgraph)
        Frame(rel, args)
      case _ => throw new IllegalArgumentException("Could not deserialize: " + pickled)
    }
  }
}

case class Relation(node: DependencyNode, name: String, sense: String) {
  require(!(name matches """.*[:\[\]].*"""))
  override def toString = name + "." + sense
  def serialize = name + "_" + node.index + "." + sense
}
object Relation {
  def fromString(node: DependencyNode, string: String) = {
    val Array(name, sense) = string.split("\\.")
    Relation(node, name, sense)
  }

  def deserialize(dgraph: DependencyGraph)(pickled: String) = {
    val Array(label, sense) = pickled.split("\\.")
    val Array(name, nodeIndex) = label.split("_")

    val node = dgraph.nodes.find(_.index == nodeIndex.toInt).get
    Relation(node, name, sense)
  }
}

case class Argument(node: DependencyNode, role: Role) {
  override def toString = role + "=" + node.string
  def serialize = role + "=" + node.string + "_" + node.index
}
object Argument {
  def deserialize(dgraph: DependencyGraph)(pickled: String) = {
    val (roleString, rest) = pickled.span(_ != '=')
    val Array(string, nodeIndex) = rest.drop(1).split("_")
    val node = dgraph.nodes.find(_.index == nodeIndex.toInt).get
    require(node.text == string, node.text + " != " + string)
    Argument(node, Roles(roleString))
  }
}

abstract class Role(val description: String) {
  override def toString = label
  def label = this.getClass.getSimpleName.replaceAll("_", "-").takeWhile(_ != '$')
}
object Roles {
  def apply(label: String) = {
    val APattern = """A(\d+)""".r
    val CPattern = """C-A(\d+)""".r
    val RPattern = """R-A(\d+)""".r
    label match {
      case "A0" => A0
      case "A1" => A1
      case "A2" => A2
      case "A3" => A3
      case "A4" => A4
      case "A5" => A5
      case CPattern(n) => C(n.toInt)
      case RPattern(n) => R(n.toInt)
      case "AM-ADV" => AM_ADV
      case "AM-DIR" => AM_DIR
      case "AM-DIS" => AM_DIS
      case "AM-EXT" => AM_EXT
      case "AM-LOC" => AM_LOC
      case "AM-MNR" => AM_MNR
      case "AM-MOD" => AM_MOD
      case "AM-NEG" => AM_NEG
      case "AM-PRD" => AM_PRD
      case "AM-PRP" => AM_PRP
      case "AM-REC" => AM_REC
      case "AM-TMP" => AM_TMP
      case "C" => C_ARG
      case "R" => R_ARG
      case _ => UnknownRole(label)
    }
  }
  case object A0 extends Role("subject")
  case object A1 extends Role("object")
  case object A2 extends Role("indirect object")
  case object A3 extends Role("???")
  case object A4 extends Role("???")
  case object A5 extends Role("???")
  case class C(n: Int) extends Role("continuation")
  case class R(n: Int) extends Role("reference")
  case object AM_ADV extends Role("adverbial modification")
  case object AM_DIR extends Role("direction")
  case object AM_DIS extends Role("discourse marker")
  case object AM_EXT extends Role("extent")
  case object AM_LOC extends Role("location")
  case object AM_MNR extends Role("manner")
  case object AM_MOD extends Role("general modification")
  case object AM_NEG extends Role("negation")
  case object AM_PRD extends Role("secondary predicate")
  case object AM_PRP extends Role("purpose")
  case object AM_REC extends Role("recipricol")
  case object AM_TMP extends Role("temporal")
  case object C_ARG extends Role("continuity of an argument/adjunct of type arg")
  case object R_ARG extends Role("reference to an actual argument/adjunct of type arg")
  case class UnknownRole(override val label: String) extends Role(label)
}
