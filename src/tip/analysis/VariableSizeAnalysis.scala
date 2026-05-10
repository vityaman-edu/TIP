package tip.analysis

import tip.ast.ADeclaration
import tip.cfg.IntraproceduralProgramCfg
import tip.ast.AstNodeData.DeclarationData
import tip.lattices.IntervalLattice
import tip.cfg.CfgNode

object TypeElement extends Enumeration {
  val Bool, Byte, Char, Int, BigInt, Any, Bot = Value
}

object VariableSizeAnalysis {
  type IntervalAnalysis = ((IntraproceduralProgramCfg, DeclarationData) => Map[CfgNode, Map[ADeclaration, IntervalLattice.Element]])
}

class VariableSizeAnalysis(cfg: IntraproceduralProgramCfg, interval: VariableSizeAnalysis.IntervalAnalysis)(implicit declData: DeclarationData)
    extends Analysis[Map[ADeclaration, TypeElement.Value]] {

  override def analyze(): Map[ADeclaration, TypeElement.Value] =
    interval(cfg, declData).values
      .flatMap(_.toList)
      .groupBy(_._1)
      .mapValues(_.map(_._2).foldLeft(IntervalLattice.bottom)(IntervalLattice.lub))
      .mapValues(typeFor)

  def typeFor(x: IntervalLattice.Element): TypeElement.Value = {
    val (a, b) = x
    if ((a, b) == IntervalLattice.bottom) {
      TypeElement.Any
    } else if (0 <= a && b <= 1) {
      TypeElement.Bool
    } else if (-128 <= a && b <= 127) {
      TypeElement.Byte
    } else if (0 <= a && b <= (1 << 16) - 1) {
      TypeElement.Char
    } else if (-(1 << 31) <= a && b <= (1 << 31) - 1) {
      TypeElement.Int
    } else {
      TypeElement.BigInt
    }
  }
}
