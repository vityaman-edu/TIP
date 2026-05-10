package tip.analysis

import tip.cfg.IntraproceduralProgramCfg
import tip.ast.AstNodeData.DeclarationData
import tip.solvers.SimpleMapLatticeFixpointSolver
import tip.solvers.SimpleWorklistFixpointSolver
import tip.cfg.CfgNode
import tip.lattices.MapLattice
import tip.ast.NoPointers
import tip.ast.NoRecords
import tip.lattices.ReversePowersetLattice
import tip.ast.AExpr
import tip.ast.AstOps.UnlabelledNode
import tip.cfg.CfgFunExitNode
import tip.cfg.CfgStmtNode
import tip.ast.AAssignStmt
import tip.ast.AIdentifier
import tip.ast.AOutputStmt
import tip.ast.AReturnStmt

abstract class VeryBusyExpAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {
  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val allExps: Set[UnlabelledNode[AExpr]] = cfg.nodes.flatMap(_.appearingNonInputExpressions.map(UnlabelledNode[AExpr]))

  val lattice: MapLattice[CfgNode, ReversePowersetLattice[UnlabelledNode[AExpr]]] = new MapLattice(new ReversePowersetLattice(allExps))

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.top
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case x: AIdentifier =>
                s.filter { e =>
                  !(x.appearingIds subsetOf e.n.appearingIds)
                } union
                  as.right.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
              case _ => ???
            }
          case exp: AExpr =>
            s union exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case out: AOutputStmt =>
            s union out.exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case ret: AReturnStmt =>
            s union ret.exp.appearingNonInputExpressions.map(UnlabelledNode[AExpr])
          case _ => s
        }
      case _ => s
    }
}

class VeryBusyExpAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends VeryBusyExpAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

class VeryBusyExpAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends VeryBusyExpAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
