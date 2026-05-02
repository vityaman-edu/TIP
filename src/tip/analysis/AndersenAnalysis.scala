package tip.analysis

import tip.ast.ADeclaration
import tip.ast.AstNodeData.DeclarationData
import tip.ast.DepthFirstAstVisitor
import tip.ast._
import tip.solvers._
import tip.util.Log

import scala.language.implicitConversions

class AndersenAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Unit] with PointsToAnalysis {

  val log = Log.logger[this.type]()

  sealed trait Cell
  case class Alloc(alloc: AAlloc) extends Cell {
    override def toString = s"alloc-${alloc.loc}"
  }
  case class Var(id: ADeclaration) extends Cell {
    override def toString = id.toString
  }

  val solver = new SimpleCubicSolver[Cell, Cell]

  import AstOps._
  val cells: Set[Cell] = (program.appearingIds.map(Var): Set[Cell]) union program.appearingAllocs.map(Alloc)

  NormalizedForPointsToAnalysis.assertContainsProgram(program)
  NoRecords.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze(): Unit =
    visit(program, ())

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Unit): Unit = {

    implicit def identifierToTarget(id: AIdentifier): Var = Var(id)
    implicit def allocToTarget(alloc: AAlloc): Alloc = Alloc(alloc)

    node match {
      case AAssignStmt(_: AIdentifier, _: AAlloc, _) => ??? //<--- Complete here
      case AAssignStmt(_: AIdentifier, AVarRef(_: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(_: AIdentifier, _: AIdentifier, _) => ??? //<--- Complete here
      case AAssignStmt(_: AIdentifier, AUnaryOp(DerefOp, _: AIdentifier, _), _) => ??? //<--- Complete here
      case AAssignStmt(ADerefWrite(_: AIdentifier, _), _: AIdentifier, _) => ??? //<--- Complete here
      case _ =>
    }
    visitChildren(node, ())
  }

  /**
    * @inheritdoc
    */
  def pointsTo(): Map[ADeclaration, Set[AstNode]] = {
    val pointsTo = solver.getSolution.collect {
      case (v: Var, ts: Set[Cell]) =>
        v.id -> ts.map {
          case Var(x) => x
          case Alloc(m) => m
        }
    }
    log.info(s"Points-to:\n${pointsTo.mapValues(v => s"{${v.mkString(",")}}").mkString("\n")}")
    pointsTo
  }

  /**
    * @inheritdoc
    */
  def mayAlias(): (ADeclaration, ADeclaration) => Boolean = { (x: ADeclaration, y: ADeclaration) =>
    solver.getSolution(Var(x)).intersect(solver.getSolution(Var(y))).nonEmpty
  }
}
