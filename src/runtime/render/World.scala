// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

import runtime._

abstract class World {
  // Return the list of boundaries where this world-space ray
  // crosses shapes in the world.
  def boundaries(ray: Ray): List[Boundary]
}

class Union(val left: World, val right: World) extends World {
  def boundaries(ray: Ray) =
    Boundary.union(left.boundaries(ray), right.boundaries(ray))
}

class Intersect(val left: World, val right: World) extends World {
  def boundaries(ray: Ray) =
    Boundary.intersect(left.boundaries(ray), right.boundaries(ray))
}

class Difference(val left: World, val right: World) extends World {
  def boundaries(ray: Ray) =
    Boundary.difference(left.boundaries(ray), right.boundaries(ray))
}

object World {
  def make(view: View, scene: Scene): World =
    scene match {
      case Sphere(surface)   => new Sphere(view, surface)
      case Cube(surface)     => new Cube(view, surface)
      case Cylinder(surface) => new Cylinder(view, surface)
      case Cone(surface)     => new Cone(view, surface)
      case Plane(surface)    => new Plane(view, surface)

      case Union(left, right) =>
        new Union(make(view, left), make(view, right))
      case Intersect(left, right) =>
        new Intersect(make(view, left), make(view, right))
      case Difference(left, right) =>
        new Difference(make(view, left), make(view, right))

      case RotateX(scene0, degrees) => make(view.rotateX(degrees), scene0)
      case RotateY(scene0, degrees) => make(view.rotateY(degrees), scene0)
      case RotateZ(scene0, degrees) => make(view.rotateZ(degrees), scene0)
      case Translate(scene0, dx, dy, dz) =>
        make(view.translate(dx, dy, dz), scene0)
      case Scale(scene0, rx, ry, rz) => make(view.scale(rx, ry, rz), scene0)
      case UScale(scene0, r)         => make(view.uScale(r), scene0)
    }
}
