// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import collection.mutable.HashSet

class Liveness(function: FunctionTerm) {
  // Have we seen a (non-tail) call?
  var seenCall = false

  // Have we seen a call with nonlocal references after it?
  var seenCallWithRestore = false

  // Have we seen a nonlocal reference?
  var seenNonlocal = false

  // Have we seen a nonlocal reference since the last call?
  var seenNonlocalSinceCall = false

  // Have we seen a local reference?
  var seenLocal = false

  // Currently live locals.
  val liveLocals = new HashSet[String]

  // Do the analysis.
  def analyse: Unit = {
    function.body.lastOption match {
      case Some(c @ Call(_)) => c.isTailCall = true
      case _ =>
    }
    traverse(function.body)
  }

  private def traverse(body: Array[Term]): Unit =
    for (term <- body.reverse) term match {
      case nested @ FunctionTerm(_)   => handleNested(nested)
      case ArrayCtor(arrayBody)       => traverse(arrayBody)
      case id @ Identifier(_)         => handleIdentifier(id)
      case b @ Binder(_)              => handleBinder(b)
      case c @ Call(_)                => handleCall(c)
      case _                          =>
    }

  private def handleNested(nested: FunctionTerm): Unit = {
    for (name <- nested.parentLocals if !(liveLocals contains name)) {
      // Closure construction is the last to use this local.
      nested.localsToFree = name :: nested.localsToFree
      liveLocals + name
      maybeLeaveFrame(nested)
    }

    if (nested.hasParentNonlocals) {
      seenNonlocal = true
      seenNonlocalSinceCall = true
    }
  }

  private def handleIdentifier(id: Identifier): Unit =
    if (id.isLocal) {
      if (!(liveLocals contains id.name)) {
        // Last reference to this local.
        id.freeLocal = true
        liveLocals + id.name
        maybeLeaveFrame(id)
      }
    } else {
      seenNonlocal = true
      seenNonlocalSinceCall = true
    }

  private def handleBinder(b: Binder): Unit =
    if (liveLocals contains b.name) liveLocals - b.name
    else b.isUnused = true

  private def handleCall(c: Call): Unit =
    if (!c.isTailCall) {
      // Restore returnPointer if this is the last call.
      if (!seenCall) {
        seenCall = true
        c.restoreReturnPointer = true
        maybeLeaveFrame(c)
      }

      // Save currentContext if there are nonlocal references after the call.
      if (seenNonlocal) c.saveCurrentContext = true

      // Restore currentContext if nonlocals are referenced before the next
      // call.  Free the currentContext slot on the last such occasion.
      if (seenNonlocalSinceCall) {
        seenNonlocalSinceCall = false
        c.restoreCurrentContext = true
        if (!seenCallWithRestore) {
          seenCallWithRestore = true
          c.freeCurrentContext = true
        }
      }
    }

  private def maybeLeaveFrame(term: Term): Unit =
    if (!seenLocal) {
      seenLocal = true
      term.leaveFrame = true
    }
}
