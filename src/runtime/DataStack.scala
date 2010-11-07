// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

class DataStack {
  // Data stack.
  var stack = new Array[Value](256)
  var stackTop = 0
  var stackBottom = 0

  def push(value: Value): Unit = {
    stack = ExpandArray.set(stack, stackTop, value)
    stackTop += 1
  }

  def pop: Value = {
    if (stackTop == stackBottom) throw new Exception("gml: stack underflow")
    stackTop -= 1
    stack(stackTop)
  }

  // Meta-stack.
  var metaStack = new Array[Int](8)
  var metaStackTop = 0

  def startArray: Unit = {
    metaStack = ExpandArray.set(metaStack, metaStackTop, stackBottom)
    metaStackTop += 1
    stackBottom = stackTop
  }

  def endArray: Unit = {
    val elements = stack.slice(stackBottom, stackTop)
    stackTop = stackBottom
    if (metaStackTop == 0) throw new Exception("gml: meta stack underflow")
    metaStackTop -= 1
    stackBottom = metaStack(metaStackTop)
    push(ArrayValue(elements))
  }
}
