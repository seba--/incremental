package constraints

trait Constraint[G <: GenBase, C] {
  def solve[CS <: ConstraintSystem[G, C, CS]](cs: CS): CS
}
