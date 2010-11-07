// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

class Cone(view: View, surface: ClosureValue) extends Shape(view, surface) {
  def shapeBoundaries(ray: Ray) = {
    // Solve ray(time) conjY ray(time) = 0
    val a = ray.velocity conjY ray.velocity
    val b = ray.origin conjY ray.velocity
    val c = ray.origin conjY ray.origin
    val list = Quadratic.solve(a, b, c) match {
      case Quadratic.NoRoots => Nil
      case Quadratic.OneRoot(root) =>
        // Parallel to the cone.
        val list0 = List(bound(ray, root, 0))
        if (b < 0) Boundary.intersect(univ(ray), list0) // entering the cone
        else Boundary.difference(univ(ray), list0)      // leaving the cone
      case Quadratic.DoubleRoot(root) =>
        // Tangent to the cone.
        if (a < 0) univ(ray)                            // inside the cone
        else Nil                                        // outside the cone
      case Quadratic.TwoRoots(low, high) =>
        val list0 = List(bound(ray, low, 0), bound(ray, high, 0))
        if (a > 0) Boundary.intersect(univ(ray), list0) // across the cone axis
        else Boundary.difference(univ(ray), list0)      // along the cone axis
    }
    // face 1: y = 1
    unitDim(list, ray, ray.origin.y, ray.velocity.y, 0, 1)
  }

  def shapeNormal(face: Int, surfacePoint: Point) =
    if (face == 1) Point(0, 1, 0)
    else Point(surfacePoint.x, -surfacePoint.y, surfacePoint.z)

  def faceCoordinates(face: Int, surfacePoint: Point) = {
    val u = if (face == 0) Shape.theta(surfacePoint.x, surfacePoint.z)
            else (surfacePoint.x + 1) / 2
    val v = if (face == 0) surfacePoint.y
            else (surfacePoint.z + 1) / 2
    (u, v)
  }
}
