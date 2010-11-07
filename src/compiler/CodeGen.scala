// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import collection.mutable.Stack
import runtime._

class CodeGen(function: FunctionTerm, code: Stack[Bytecode]) {
  // Generate code for the function.
  def generate: Unit = {
    function.address = code.size
    traverse(function.body)
    function.body.lastOption match {
      case Some(c @ Call(_)) if (c.isTailCall) =>
      case _ => code push Return
    }
  }

  private def traverse(body: Array[Term]): Unit =
    for (term <- body) term match {
      case nested @ FunctionTerm(_)   => generateNested(nested)
      case ArrayCtor(arrayBody)       => generateArrayCtor(arrayBody)
      case id @ Identifier(_)         => generateIdentifier(id)
      case b @ Binder(_)              => generateBinder(b)
      case c @ Call(_)                => generateCall(c)
      case Render                     => code push DoRender
      case Operator(name)             => code push Op(Operators(name))
      case IntLiteral(n)              => code push PushInt(n)
      case RealLiteral(d)             => code push PushReal(d)
      case StringLiteral(s)           => code push PushString(s)
    }

  private def generateNested(nested: FunctionTerm): Unit = {
    maybeEnterFrame(nested)
    code push PushInt(nested.address)
    nested.locations foreach { code push _.bytecode }
    code push MakeClosure(nested.locations.length)
    maybeLeaveFrame(nested)
  }

  private def generateArrayCtor(body: Array[Term]): Unit = {
    code push StartArray
    traverse(body)
    code push EndArray
  }

  private def generateIdentifier(id: Identifier): Unit = {
    code push id.location.bytecode
    maybeLeaveFrame(id)
  }

  private def generateBinder(b: Binder): Unit = {
    maybeEnterFrame(b)
    if (b.isUnused) code push PopUnused
    else code push PopLocal(b.frameSlot)
  }

  private def generateCall(c: Call): Unit = {
    maybeEnterFrame(c)
    if (c.saveReturnPointer) code push SaveReturnPtr(c.returnPointerSlot)
    if (c.saveCurrentContext) code push SaveContext(c.currentContextSlot)
    c.callType match {
      case CallType.Apply => code push Apply(c.isTailCall)
      case CallType.If    => code push If(c.isTailCall)
    }
    if (c.restoreCurrentContext) code push RestoreContext(c.currentContextSlot)
    if (c.restoreReturnPointer) code push RestoreReturnPtr(c.returnPointerSlot)
    maybeLeaveFrame(c)
  }

  private def maybeEnterFrame(term: Term): Unit =
    if (term.enterFrame) code push EnterFrame(function.frameSize)

  private def maybeLeaveFrame(term: Term): Unit =
    if (term.leaveFrame) code push LeaveFrame(function.frameSize)
}
