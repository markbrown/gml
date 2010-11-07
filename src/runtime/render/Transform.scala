// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

// Represents a 4x4 matrix:
//    [x1 y1 z1 t1]
//    [x2 y2 z2 t2]
//    [x3 y3 z3 t3]
//    [ 0  0  0  1]
case class Transform(
          x1: Double, y1: Double, z1: Double, t1: Double,
          x2: Double, y2: Double, z2: Double, t2: Double,
          x3: Double, y3: Double, z3: Double, t3: Double
        )
{
  def transformRay(ray: Ray) =
    Ray(transformPoint(ray.origin), transformVector(ray.velocity))

  def transformPoint(p: Point) =
    Point(
      x1*p.x + y1*p.y + z1*p.z + t1,
      x2*p.x + y2*p.y + z2*p.z + t2,
      x3*p.x + y3*p.y + z3*p.z + t3
    )

  def transformVector(p: Point) =
    Point(
      x1*p.x + y1*p.y + z1*p.z,
      x2*p.x + y2*p.y + z2*p.z,
      x3*p.x + y3*p.y + z3*p.z
    )

  def *(m: Transform) =
    Transform(
      x1*m.x1 + y1*m.x2 + z1*m.x3,
      x1*m.y1 + y1*m.y2 + z1*m.y3,
      x1*m.z1 + y1*m.z2 + z1*m.z3,
      x1*m.t1 + y1*m.t2 + z1*m.t3 + t1,
      x2*m.x1 + y2*m.x2 + z2*m.x3,
      x2*m.y1 + y2*m.y2 + z2*m.y3,
      x2*m.z1 + y2*m.z2 + z2*m.z3,
      x2*m.t1 + y2*m.t2 + z2*m.t3 + t2,
      x3*m.x1 + y3*m.x2 + z3*m.x3,
      x3*m.y1 + y3*m.y2 + z3*m.y3,
      x3*m.z1 + y3*m.z2 + z3*m.z3,
      x3*m.t1 + y3*m.t2 + z3*m.t3 + t3
    )
}

object Transform {
  val identity =
    Transform(1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 1, 0)

  def rotationX(deg: Double): Transform = {
    val rad = Math.toRadians(deg)
    val sin = Math.sin(rad)
    val cos = Math.cos(rad)
    Transform(1, 0,    0,   0,
              0, cos, -sin, 0,
              0, sin,  cos, 0)
  }

  def rotationY(deg: Double): Transform = {
    val rad = Math.toRadians(deg)
    val sin = Math.sin(rad)
    val cos = Math.cos(rad)
    Transform(cos, 0, sin, 0,
              0,   1, 0,   0,
             -sin, 0, cos, 0)
  }

  def rotationZ(deg: Double): Transform = {
    val rad = Math.toRadians(deg)
    val sin = Math.sin(rad)
    val cos = Math.cos(rad)
    Transform(cos, -sin, 0, 0,
              sin,  cos, 0, 0,
              0,    0,   1, 0)
  }

  def translation(dx: Double, dy: Double, dz: Double) =
    Transform(1, 0, 0, dx,
              0, 1, 0, dy,
              0, 0, 1, dz)

  def dilation(rx: Double, ry: Double, rz: Double) =
    Transform(rx, 0, 0, 0,
              0, ry, 0, 0,
              0, 0, rz, 0)
}
