// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import collection.mutable.HashMap

class SlotAlloc(function: FunctionTerm) {
  // Current slot allocation.
  val frameSlots = new HashMap[String, Int]

  // Number of slots allocated.
  var numFrameSlots = 0

  // Frame slots no longer in use.
  var freeList: List[Int] = Nil

  // Have we entered the frame?
  var frameEntered = false

  // Have we saved returnPointer?
  var savedReturnPointer = false
  val nameReturnPointer = "_returnPointer"

  // Have we saved currentContext?
  var savedCurrentContext = false
  val nameCurrentContext = "_currentContext"

  // Allocate the slots.
  def allocate: Unit = {
    traverse(function.body)
    function.frameSize = numFrameSlots
  }

  private def traverse(body: Array[Term]): Unit =
    for (term <- body) term match {
      case nested @ FunctionTerm(_)   => handleNested(nested)
      case ArrayCtor(arrayBody)       => traverse(arrayBody)
      case id @ Identifier(_)         => handleIdentifier(id)
      case b @ Binder(_)              => handleBinder(b)
      case c @ Call(_)                => handleCall(c)
      case _                          =>
    }

  private def handleNested(nested: FunctionTerm): Unit = {
    // Set the locations of the nested nonlocals.
    nested.locations = nested.nonlocals map { arg =>
      if (nested.parentLocals contains arg) FrameSlot(frameSlots(arg))
      else ContextSlot(function.nonlocalMap(arg))
    }

    // Free all parent locals whose last appearance is in the nested function.
    nested.localsToFree.foreach(freeSlot)
  }

  private def handleIdentifier(id: Identifier): Unit =
    if (id.isLocal) {
      id.location = FrameSlot(frameSlots(id.name))
      if (id.freeLocal) freeSlot(id.name)
    } else {
      id.location = ContextSlot(function.nonlocalMap(id.name))
    }

  private def handleBinder(b: Binder): Unit =
    if (!b.isUnused) {
      maybeEnterFrame(b)
      b.frameSlot = allocateSlot(b.name)
    }

  private def handleCall(c: Call): Unit =
    if (!c.isTailCall) {
      maybeEnterFrame(c)

      if (!savedReturnPointer) {
        savedReturnPointer = true
        c.saveReturnPointer = true
        c.returnPointerSlot = allocateSlot(nameReturnPointer)
      }

      if (c.saveCurrentContext) {
        if (!savedCurrentContext) {
          savedCurrentContext = true
          c.currentContextSlot = allocateSlot(nameCurrentContext)
        } else {
          c.saveCurrentContext = false
        }
      }

      if (c.restoreReturnPointer) {
        c.returnPointerSlot = frameSlots(nameReturnPointer)
        freeSlot(nameReturnPointer)
      }

      if (c.restoreCurrentContext)
        c.currentContextSlot = frameSlots(nameCurrentContext)

      if (c.freeCurrentContext)
        freeSlot(nameCurrentContext)
    }

  private def maybeEnterFrame(term: Term): Unit =
    if (!frameEntered) {
      frameEntered = true
      term.enterFrame = true
    }

  private def allocateSlot(name: String): Int = {
    var slot = -1
    freeList match {
      case firstFree :: rest =>
        slot = firstFree
        freeList = rest
      case Nil =>
        slot = numFrameSlots
        numFrameSlots += 1
    }
    frameSlots(name) = slot
    slot
  }

  private def freeSlot(name: String): Unit = {
    freeList = frameSlots(name) :: freeList
    frameSlots.removeKey(name)
  }
}
