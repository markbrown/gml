// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

class Sphere(view: View, surface: ClosureValue) extends Shape(view, surface) {
  def shapeBoundaries(ray: Ray) = {
    // Solve ray(time).squared = 1.
    val a = ray.velocity.squared
    val b = ray.origin dot ray.velocity
    val c = ray.origin.squared - 1
    Quadratic.solve(a, b, c) match {
      case Quadratic.TwoRoots(low, high) =>
        val list = List(bound(ray, low, 0), bound(ray, high, 0))
        Boundary.intersect(list, univ(ray))
      case _ => Nil
    }
  }

  def shapeNormal(face: Int, surfacePoint: Point) = surfacePoint

  def faceCoordinates(face: Int, surfacePoint: Point) =
    (Shape.theta(surfacePoint.x, surfacePoint.z), (surfacePoint.y + 1) / 2)
}
