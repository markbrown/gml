// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import collection.mutable.Stack
import runtime.Bytecode

class Compiler {
  // Code is generated here.
  val codeStack = new Stack[Bytecode]
  def code = codeStack.toArray

  // Compile a function, after recursively compiling all nested functions.
  def compile(function: FunctionTerm): Unit = {
    recursiveCompile(function.body)
    new Nonlocals(function).analyse
    new Liveness(function).analyse
    new SlotAlloc(function).allocate
    new CodeGen(function, codeStack).generate
  }

  // Compile all nested functions in the body.
  def recursiveCompile(body: Array[Term]): Unit =
    for (term <- body) term match {
      case nested @ FunctionTerm(_)   => compile(nested)
      case ArrayCtor(arrayBody)       => recursiveCompile(arrayBody)
      case _                          =>
    }
}
