// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

class Cube(view: View, surface: ClosureValue) extends Shape(view, surface) {
  def shapeBoundaries(ray: Ray) = {
    var list = univ(ray)

    // faces 0, 1: z = 0, 1
    list = unitDim(list, ray, ray.origin.z, ray.velocity.z, 0, 1)

    // faces 2, 3: x = 0, 1
    list = unitDim(list, ray, ray.origin.x, ray.velocity.x, 2, 3)

    // faces 4, 5: y = 1, 0
    unitDim(list, ray, ray.origin.y, ray.velocity.y, 5, 4)
  }

  def shapeNormal(face: Int, surfacePoint: Point) = {
    object Normal {
      val faces: Array[Point] =
        Array(
          Point(0,  0, -1),
          Point(0,  0,  1),
          Point(-1, 0,  0),
          Point(1,  0,  0),
          Point(0,  1,  0),
          Point(0, -1,  0)
        )
    }
    Normal.faces(face)
  }

  def faceCoordinates(face: Int, surfacePoint: Point) = {
    val u = if (face == 2 || face == 3) surfacePoint.z else surfacePoint.x
    val v = if (face == 4 || face == 5) surfacePoint.z else surfacePoint.y
    (u, v)
  }
}
