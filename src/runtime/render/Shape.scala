// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime.ClosureValue

abstract class Shape(val view: View, val surface: ClosureValue) extends World {
  def boundaries(ray: Ray) =
    shapeBoundaries(view.rayToShapeSpace(ray))

  // Same as boundaries, except the argument is in shape space.
  def shapeBoundaries(ray: Ray): List[Boundary]

  // Given a face and a surface point in shape coordinates, return
  // the unit normal in world coordinates.
  def normal(face: Int, surfacePoint: Point): Point = {
    val n = shapeNormal(face, surfacePoint)

    // Choose an independent vector.
    val q = if (n.x != 0 || n.y != 0) Point(0, 0, 1)
            else Point(1, 0, 0)

    // Find two perpendicular tangents in shape coordinates.
    val t1 = n cross q
    val t2 = n cross t1

    // Convert tangents to world space, which preserves their
    // tangent-hood but not their size.
    val w1 = view.vectorToWorldSpace(t1)
    val w2 = view.vectorToWorldSpace(t2)

    // Recover a unit normal in world space.
    (w1 cross w2).unit
  }

  // Given a face and a surface point in shape coordinates, return
  // a normal vector in shape coordinates.
  def shapeNormal(face: Int, surfacePoint: Point): Point

  // Convert a surface point in shape coordinates to face coordinates.
  def faceCoordinates(face: Int, surfacePoint: Point): (Double, Double)

  // Construct a boundary for the given ray in shape space, time and face.
  protected def bound(ray: Ray, time: Double, face: Int) =
    Boundary(time, ray(time), this, face)

  // Boundary list representing the whole ray.
  protected def univ(ray: Ray) =
    List(bound(ray, 0, 0))

  // Constrain one dimension by face0 at 0 and face1 at 1.
  protected def unitDim(list: List[Boundary], ray: Ray, origin: Double,
                  velocity: Double, face0: Int, face1: Int): List[Boundary] =
    if (list == Nil) Nil
    else if (velocity == 0) {
      if (origin < 0 || origin > 1) Nil
      else list
    } else {
      val bound0 = bound(ray, -origin / velocity, face0)
      val bound1 = bound(ray, (1 - origin) / velocity, face1)
      val list0 = if (velocity < 0) {
        List(bound1, bound0)
      } else {
        List(bound0, bound1)
      }
      Boundary.intersect(list0, list)
    }
}

object Shape {
  // Calculate angle in the range 0..1.
  def theta(y: Double, x: Double): Double = {
    val q = Math.atan2(y, x) / (2 * Math.Pi)
    if (q > 0) q else q + 1
  }
}
