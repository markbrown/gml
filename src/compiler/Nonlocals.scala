// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import collection.mutable.{HashSet, Stack}

class Nonlocals(function: FunctionTerm) {
  // Binders seen.
  val locals = new HashSet[String]

  // Nonlocals allocated.
  val nonlocals = new Stack[String]

  // Do the analysis.
  def analyse: Unit = {
    traverse(function.body)
    function.nonlocals = nonlocals.toArray
    function.parentLocals ++= nonlocals
  }

  private def traverse(body: Array[Term]): Unit =
    for (term <- body) term match {
      case nested @ FunctionTerm(_)   => handleNested(nested)
      case ArrayCtor(body)            => handleArrayCtor(body)
      case id @ Identifier(name)      => id.isLocal = reference(name)
      case Binder(name)               => locals + name
      case _                          =>
    }

  private def handleNested(nested: FunctionTerm): Unit =
    for (name <- nested.nonlocals if !reference(name)) {
      nested.parentLocals - name
      nested.hasParentNonlocals = true
    }

  private def handleArrayCtor(body: Array[Term]): Unit = {
    val startLocals = locals.clone
    traverse(body)
    locals intersect startLocals
  }

  private def reference(name: String): Boolean = {
    val isLocal = locals contains name
    if (!isLocal && !(function.nonlocalMap contains name)) {
      function.nonlocalMap(name) = nonlocals.size
      nonlocals.push(name)
    }
    isLocal
  }
}
