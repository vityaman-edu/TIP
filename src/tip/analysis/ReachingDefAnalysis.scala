package tip.analysis

import tip.cfg.IntraproceduralProgramCfg
import tip.lattices.MapLattice
import tip.lattices.PowersetLattice
import tip.cfg.CfgNode
import tip.ast.NoPointers
import tip.ast.NoRecords
import tip.solvers.SimpleMapLatticeFixpointSolver
import tip.solvers.SimpleWorklistFixpointSolver
import tip.cfg.CfgFunExitNode
import tip.cfg.CfgStmtNode
import tip.ast.AAssignStmt
import tip.ast.AIdentifier
import tip.ast.AstNodeData.DeclarationData

// May, Forward
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {
  import tip.ast.AstOps._

  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case x: AIdentifier => s.filter(!_.isAssignmentTo(x)) + as
              case _ => ???
            }
          case _ => s
        }
      case _ => s
    }
}

class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
