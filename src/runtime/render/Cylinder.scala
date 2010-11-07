// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

class Cylinder(view: View, surface: ClosureValue) extends Shape(view, surface)
{
  def shapeBoundaries(ray: Ray) = {
    // Solve ray(time) dotXZ ray(time) = 1
    val a = ray.velocity dotXZ ray.velocity
    val b = ray.origin dotXZ ray.velocity
    val c = (ray.origin dotXZ ray.origin) - 1
    val list = Quadratic.solve(a, b, c) match {
      case Quadratic.TwoRoots(low, high) =>
        val list0 = List(bound(ray, low, 0), bound(ray, high, 0))
        Boundary.intersect(list0, univ(ray))
      case Quadratic.NoRoots if (c < 0) => univ(ray)
      case _ => Nil
    }
    // face 1, 2: y = 1, 0
    unitDim(list, ray, ray.origin.y, ray.velocity.y, 2, 1)
  }

  def shapeNormal(face: Int, surfacePoint: Point) =
    if (face == 1) Point(0, 1, 0)
    else if (face == 2) Point(0, -1, 0)
    else Point(surfacePoint.x, 0, surfacePoint.z)

  def faceCoordinates(face: Int, surfacePoint: Point) = {
    val u = if (face == 0) Shape.theta(surfacePoint.x, surfacePoint.z)
            else (surfacePoint.x + 1) / 2
    val v = if (face == 0) surfacePoint.y
            else (surfacePoint.z + 1) / 2
    (u, v)
  }
}
