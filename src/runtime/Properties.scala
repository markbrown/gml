// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

import render.Point

case class Properties(color: Point, kd: Double, ks: Double, exp: Double)

object Properties {
  def properties(vm: VM, surface: ClosureValue, face: Int,
                 u: Double, v: Double): Properties =
  {
    // Save registers.
    val savedContext = vm.currentContext
    val savedCodePointer = vm.codePointer
    val savedReturnPointer = vm.returnPointer

    // Perform call.
    vm.data.push(IntValue(face))
    vm.data.push(RealValue(u))
    vm.data.push(RealValue(v))
    vm.currentContext = surface.context
    vm.execute(surface.address)
    val exp = vm.data.pop.toReal
    val ks = vm.data.pop.toReal
    val kd = vm.data.pop.toReal
    val color = vm.data.pop.toPoint

    // Restore registers.
    vm.returnPointer = savedReturnPointer
    vm.codePointer = savedCodePointer
    vm.currentContext = savedContext

    // Return the properties.
    Properties(color, kd, ks, exp)
  }
}
