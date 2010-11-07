// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import Math.{cos, pow, toRadians}

sealed abstract class Light {
  // The color of the light.
  val color: Point

  // Return a unit vector pointing towards the light from point.
  def directionFrom(point: Point): Point

  // Attenuation of the light at a point, given the unit ray pointing
  // towards the light.
  def attenuation(ray: Ray): Double

  // Is the light blocked at point by an object at the given distance
  // squared?
  def isBlocked(point: Point, dSqr: Double): Boolean
}

case class PlainLight(direction: Point, color: Point) extends Light {
  // Unit vector pointing directly towards the light.
  val towards = direction.neg.unit
  def directionFrom(point: Point) = towards
  def attenuation(ray: Ray) = 1
  def isBlocked(point: Point, dSqr: Double) = true
}

case class PointLight(position: Point, color: Point) extends Light {
  def directionFrom(point: Point) = (position - point).unit
  def attenuation(ray: Ray) = 100 / (99 + (position - ray.origin).squared)
  def isBlocked(point: Point, dSqr: Double) = (position - point).squared > dSqr
}

case class SpotLight(
              override val position: Point,
              at: Point,
              override val color: Point,
              cutoff: Double,
              exponent: Double
           ) extends PointLight(position, color)
{
  // Unit vector pointing directly towards the light.
  val towards = (position - at).unit

  // Cosine of the cutoff angle.
  val cosCutoff = cos(toRadians(cutoff))

  override def attenuation(ray: Ray) = {
    val cos = ray.velocity dot towards
    if (cos < cosCutoff) 0
    else pow(cos, exponent) * super.attenuation(ray)
  }
}
