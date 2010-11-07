// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import Math.{abs, pow}
import runtime.Properties

class Illumination(render: Render,
                   ray: Ray,
                   boundary: Boundary,
                   properties: Properties)
{
  val black = Point(0, 0, 0)
  val point = boundary.worldPoint
  val normal = boundary.shape.normal(boundary.face, boundary.shapePoint)
  val velocityDotNormal = ray.velocity dot normal
  val reflection = Ray(point, ray.velocity - (normal * velocityDotNormal * 2))

  // Illumination at this point from a given light.
  def fromLight(light: Light): Point = {
    val shadowRay = Ray(point, light.directionFrom(point))
    if (velocityDotNormal * (shadowRay.velocity dot normal) >= 0) {
      // The light source is below the horizon.
      return black
    }

    val attenuation = light.attenuation(shadowRay)
    if (attenuation < render.cutoff) {
      // There is not enough light to bother.
      return black
    }

    render.fireShadowRay(shadowRay) match {
      case Some(dSqr) if (light.isBlocked(point, dSqr)) =>
        // The light is blocked by something.
        return black
      case _ =>
    }

    val halfway = (shadowRay.velocity - ray.velocity).unit
    val specular = pow(abs(normal dot halfway), properties.exp) * properties.ks
    val diffuse = abs(normal dot shadowRay.velocity) * properties.kd
    light.color * (specular + diffuse) * attenuation
  }
}
