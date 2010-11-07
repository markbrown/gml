// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import Math.{tan, toRadians}
import runtime._

class Render(vm: VM,
             world: World,
             ambient: Point,
             lights: Array[Light],
             maxDepth: Int)
{
  // What is the color if there is no object hit?
  val background = Point(0, 0, 0)

  // Don't consider color components less than this value.
  val cutoff = 0.001

  // Return the square of the distance to the nearest entry, or None
  // if there is no such boundary.
  def fireShadowRay(ray: Ray): Option[Double] =
    Boundary.firstEntry(world.boundaries(ray)) map
            { b => (b.worldPoint - ray.origin).squared }

  // Return the color seen in the direction of this ray,
  // assuming we are at the given recursion depth.
  def fireRay(ray: Ray, depth: Int): Point =
    if (depth > maxDepth) background
    else Boundary.firstEntry(world.boundaries(ray)) match {
      case None => background
      case Some(boundary) =>
        val (u, v) = boundary.faceCoordinates
        val properties = Properties.properties(vm, boundary.shape.surface,
                                               boundary.face, u, v)
        val illumination = new Illumination(this, ray, boundary, properties)
        val ambientReflection = ambient * properties.kd
        val specularReflection =
          if (properties.ks * properties.color.maxCoordinate < cutoff) {
            background
          } else {
            fireRay(illumination.reflection, depth + 1) * properties.ks
          }
        var lighting = ambientReflection + specularReflection
        for (light <- lights) lighting += illumination.fromLight(light)
        lighting times properties.color
  }

  // Render the image.
  def render(fieldOfView: Double, width: Int, height: Int, filename: String) = {
    val imageWidth = 2 * tan(0.5 * toRadians(fieldOfView))
    val pixelSize = imageWidth / width
    val imageHeight = pixelSize * height
    val imageLeft = -imageWidth / 2
    val imageTop = imageHeight / 2
    val origin = Point(0, 0, -1)
    val pixmap = new Pixmap(width, height)

    // Main loop.
    for (j <- 0 until height) {
      for (i <- 0 until width) {
        val x = imageLeft + (i + 0.5) * pixelSize
        val y = imageTop - (j + 0.5) * pixelSize
        val ray = Ray(origin, Point(x, y, 1).unit)
        pixmap(i, j) = fireRay(ray, 1)
      }
      if (j % 8 == 7) print(".")
    }
    println("")

    // output
    pixmap.output(filename)
  }
}
