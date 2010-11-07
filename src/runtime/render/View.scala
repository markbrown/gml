// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import Transform._

case class View(s2w: Transform, w2s: Transform) {
  def rayToWorldSpace(ray: Ray) = s2w.transformRay(ray)
  def rayToShapeSpace(ray: Ray) = w2s.transformRay(ray)

  def pointToWorldSpace(p: Point) = s2w.transformPoint(p)
  def pointToShapeSpace(p: Point) = w2s.transformPoint(p)

  def vectorToWorldSpace(p: Point) = s2w.transformVector(p)
  def vectorToShapeSpace(p: Point) = w2s.transformVector(p)

  def rotateX(deg: Double) =
    View(s2w * rotationX(deg), rotationX(-deg) * w2s)

  def rotateY(deg: Double) =
    View(s2w * rotationY(deg), rotationY(-deg) * w2s)

  def rotateZ(deg: Double) =
    View(s2w * rotationZ(deg), rotationZ(-deg) * w2s)

  def translate(dx: Double, dy: Double, dz: Double) =
    View(s2w * translation(dx, dy, dz), translation(-dx, -dy, -dz) * w2s)

  def scale(rx: Double, ry: Double, rz: Double) =
    View(s2w * dilation(rx, ry, rz), dilation(1/rx, 1/ry, 1/rz) * w2s)

  def uScale(r: Double) = scale(r, r, r)
}

object View {
  val identity = View(Transform.identity, Transform.identity)
}
