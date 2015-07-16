package algebra

import scala.language.implicitConversions

trait Cotype[T, CS, RS, CT] {
  def cotype(t: T, cs: CS, rs: RS): CT
//  def cotypePropagate(sub: CT*): CT
  
  def cotypeT(ct: CT): T
  def cotypeCS(ct: CT): CS
  def cotypeRS(ct: CT): RS
}

object Cotype {
  def cotypeOps[T, CS, RS, CT](alg: Cotype[T, CS, RS, CT]) = new CotypeOps(alg)
}
class CotypeOps[T, CS, RS, CT](val alg: Cotype[T, CS, RS, CT]) extends AnyVal {
  implicit def opsInternal(ct: CT) = new CotypeOpsInternal(alg, ct)
}
class CotypeOpsInternal[T, CS, RS, CT](alg: Cotype[T, CS, RS, CT], ct: CT) {
  def t = alg.cotypeT(ct)
  def cs = alg.cotypeCS(ct)
  def rs = alg.cotypeRS(ct)
}