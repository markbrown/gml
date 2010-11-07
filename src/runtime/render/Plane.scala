// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

class Plane(view: View, surface: ClosureValue) extends Shape(view, surface) {
  def shapeBoundaries(ray: Ray) =
    if (ray.velocity.y == 0) {
      if (ray.origin.y >= 0) Nil
      else univ(ray)
    } else {
      val list0 = List(bound(ray, -ray.origin.y / ray.velocity.y, 0))
      if (ray.velocity.y < 0) Boundary.intersect(univ(ray), list0)
      else Boundary.difference(univ(ray), list0)
    }

  def shapeNormal(face: Int, surfacePoint: Point) = Point(0, 1, 0)

  def faceCoordinates(face: Int, surfacePoint: Point) =
    (surfacePoint.x, surfacePoint.z)
}
