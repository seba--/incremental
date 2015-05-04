package incremental.systemfomega

trait Kind

case object KStar extends Kind

case class KArrow(k1: Kind, k2: Kind) extends Kind