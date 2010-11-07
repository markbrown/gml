// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

sealed abstract class Bytecode

case object DoRender extends Bytecode
case class Apply(isTailCall: Boolean) extends Bytecode
case class If(isTailCall: Boolean) extends Bytecode
case object Return extends Bytecode
case class EnterFrame(size: Int) extends Bytecode
case class LeaveFrame(size: Int) extends Bytecode
case class SaveContext(slot: Int) extends Bytecode
case class RestoreContext(slot: Int) extends Bytecode
case class SaveReturnPtr(slot: Int) extends Bytecode
case class RestoreReturnPtr(slot: Int) extends Bytecode
case class MakeClosure(size: Int) extends Bytecode
case class PushInt(n: Int) extends Bytecode
case class PushReal(d: Double) extends Bytecode
case class PushString(s: String) extends Bytecode
case class PushNonlocal(slot: Int) extends Bytecode
case class PushLocal(slot: Int) extends Bytecode
case class PopLocal(slot: Int) extends Bytecode
case object PopUnused extends Bytecode
case class Op(op: Operators.OpType) extends Bytecode
case object StartArray extends Bytecode
case object EndArray extends Bytecode
