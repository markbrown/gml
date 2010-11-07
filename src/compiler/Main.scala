// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import io.Source
import runtime._

object Main {
  def main(args: Array[String]): Unit = {
    // Get the input source.
    val in = if (args.length > 0) Source.fromFile(args(0))
             else Source.fromInputStream(System.in)

    // Parse the input and wrap the resulting terms in a function.
    val main = FunctionTerm(GmlParser(in.getLines.mkString))

    // Compile to bytecode.
    val compiler = new Compiler
    compiler.compile(main)

    // At the top level, it is an error if there are any nonlocals.
    if (main.nonlocals.length > 0) {
      println("Error: unbound identifier(s): "+ main.nonlocals.mkString(" "))
      exit(1)
    } else {
      // Execute the bytecode.
      val vm = new VM(compiler.code)
      try {
        vm.execute(main.address)
      } catch {
        case e =>
          println(e.getMessage)
          // Close off any unfinished arrays.
          while (vm.data.metaStackTop != 0) vm.data.endArray
      }
      println("Data stack:")
      while (vm.data.stackTop > 0) println(vm.data.pop)
    }
  }
}
