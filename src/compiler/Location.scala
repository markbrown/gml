// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import runtime.{Bytecode, PushLocal, PushNonlocal}

sealed abstract class Location {
  def bytecode: Bytecode
}

case class FrameSlot(slot: Int) extends Location {
  def bytecode = PushLocal(slot)
}

case class ContextSlot(slot: Int) extends Location {
  def bytecode = PushNonlocal(slot)
}
