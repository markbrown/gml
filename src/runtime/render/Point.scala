// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

case class Point(x: Double, y: Double, z: Double) {
  import Math.{max, sqrt}

  // vector sum, difference
  def +(p: Point) = Point(x+p.x, y+p.y, z+p.z)
  def -(p: Point) = Point(x-p.x, y-p.y, z-p.z)

  // scalar product, quotient
  def *(s: Double) = Point(x*s, y*s, z*s)
  def /(s: Double) = *(1/s)

  // vector dot product
  def dot(p: Point) = x*p.x + y*p.y + z*p.z

  // vector dot product on XZ plane
  def dotXZ(p: Point) = x*p.x + z*p.z

  // vector dot product with Y-conjugate
  def conjY(p: Point) = x*p.x - y*p.y + z*p.z

  // vector cross product
  def cross(p: Point) = Point(y*p.z - z*p.y, z*p.x - x*p.z, x*p.y - y*p.x)

  // coordinate-wise product
  def times(p: Point) = Point(x*p.x, y*p.y, z*p.z)

  // maximum value of any coordinate
  def maxCoordinate = max(x, max(y, z))

  // length squared
  def squared = dot(this)

  // additive inverse
  def neg = *(-1)

  // normalize
  def unit = this / sqrt(this squared)
}
