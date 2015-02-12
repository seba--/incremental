package tasks

import data.{IList, Exp}


/**
 * @author Mirko Köhler
 */
class TaskInterpRegExp(val exp : Exp, val s : IList[Char]) extends Task[IList[IList[Char]]](exp, s){

	override def canBeRecomputed: Boolean = true
}

/*
	def interp(e : Exp, c : Text): InterpValue = e match {
		case Terminal(s2) => if (c.startsWith(s2)) Set(c.substring(s2.length)) else Set()
		case Alt(r1, r2) => interp(r1, c) ++ interp(r2, c)
		case Asterisk(r) => interp(Sequence(r, Asterisk(r)),c) + c
		case Sequence(r1, r2) => interp(r1, c) flatMap (s2 => interp(r2, s2))
	}

	def interpNorm(e : (Exp, Text)) : InterpValue = {
		if (e._1.isInstanceOf[Terminal] && e._2.startsWith(e._1.asInstanceOf[Terminal].s)) {
			val t1 : Terminal = e._1.asInstanceOf[Terminal]
			val res : InterpValue = Set(e._2.substring(t1.s.length))
			return res
		} else if (e._1.isInstanceOf[Terminal] && !(e._2.startsWith(e._1.asInstanceOf[Terminal].s))) {
			val res : InterpValue = Set()
			return res
		} else if (e._1.isInstanceOf[Alt]) {
			val t1 : Alt = e._1.asInstanceOf[Alt]
			val v1 : InterpValue = interpNorm((t1.r1,e._2)) //Task1
			val v2 : InterpValue = interpNorm((t1.r2,e._2)) //Task2
			val res : InterpValue = v1 ++ v2
			return res
		} else if (e._1.isInstanceOf[Asterisk]) {
			val t1 : Asterisk = e._1.asInstanceOf[Asterisk]
			val v1 : InterpValue = interpNorm((Sequence(t1.r1, t1),e._2)) //Task3
			val res : InterpValue = v1 + e._2
			return res
		} else if (e._1.isInstanceOf[Sequence]) {
			val t1 : Sequence = e._1.asInstanceOf[Sequence]
			val v1 : InterpValue = interpNorm((t1.r1,e._2)) //Task5
		//	val f : Function[String,InterpValue] = s => interpNorm((t1.r2,s))
		//	val v2 : InterpValue = v1 flatMap f //Task6
			val v2 : InterpValue = flatMapInterpNorm(v1,t1.r2)
			val res : InterpValue = v2
			return res
		}

		return null
	}

 */
