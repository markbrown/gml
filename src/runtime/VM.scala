// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

import render.{Render, World, View}

class VM(code: Array[Bytecode]) {
  // Stacks.
  val data = new DataStack
  val frames = new Frames

  // Registers.
  var currentContext: Array[Value] = null
  var codePointer = -1
  var returnPointer = -1

  def execute(address: Int): Unit = {
    codePointer = address
    returnPointer = -1
    while (codePointer >= 0) {
      val bytecode = code(codePointer)
      codePointer += 1
      bytecode match {
        case DoRender               => render
        case Apply(isTailCall)      => callClosure(data.pop, isTailCall)
        case If(isTailCall)         => callIf(isTailCall)
        case Return                 => codePointer = returnPointer
        case EnterFrame(size)       => frames.enter(size)
        case LeaveFrame(size)       => frames.leave(size)
        case SaveContext(slot)      => frames(slot) = ArrayValue(currentContext)
        case RestoreContext(slot)   => currentContext = frames(slot).toArray
        case SaveReturnPtr(slot)    => frames(slot) = IntValue(returnPointer)
        case RestoreReturnPtr(slot) => returnPointer = frames(slot).toInt
        case MakeClosure(size)      => makeClosure(size)
        case PushInt(n)             => data.push(IntValue(n))
        case PushReal(d)            => data.push(RealValue(d))
        case PushString(s)          => data.push(StringValue(s))
        case PushNonlocal(slot)     => data.push(currentContext(slot))
        case PushLocal(slot)        => data.push(frames(slot))
        case PopLocal(slot)         => frames(slot) = data.pop
        case PopUnused              => data.pop
        case Op(op)                 => op(data)
        case StartArray             => data.startArray
        case EndArray               => data.endArray
      }
    }
  }

  private def render: Unit = {
    val filename = data.pop.toStr
    val height = data.pop.toInt
    val width = data.pop.toInt
    val fieldOfView = data.pop.toReal
    val depth = data.pop.toInt
    val world = World.make(View.identity, data.pop.toScene)
    val lights = data.pop.toArray map { _ toLight }
    val ambient = data.pop.toPoint
    new Render(this, world, ambient, lights, depth).
            render(fieldOfView, width, height, filename)
  }

  private def callClosure(value: Value, isTailCall: Boolean): Unit = {
    val closure = value.toClosure
    if (!isTailCall) returnPointer = codePointer
    codePointer = closure.address
    currentContext = closure.context
  }

  private def callIf(isTailCall: Boolean): Unit = {
    val elseVal = data.pop
    val thenVal = data.pop
    val condition = data.pop.toBoolean
    callClosure(if (condition) thenVal else elseVal, isTailCall)
  }

  private def makeClosure(size: Int): Unit = {
    val context = new Array[Value](size)
    for (i <- (0 until size).reverse) context(i) = data.pop
    data.push(ClosureValue(data.pop.toInt, context))
  }
}
