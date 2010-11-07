// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

case class Ray(origin: Point, velocity: Point) {
  // Where will the ray extend to after the given time?
  def apply(time: Double): Point = origin + velocity * time

  // Return the ray with normalized velocity.
  def unit = Ray(origin, velocity.unit)
}
