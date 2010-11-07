// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

sealed abstract class Scene

case class Sphere(surface: ClosureValue) extends Scene
case class Cube(surface: ClosureValue) extends Scene
case class Cylinder(surface: ClosureValue) extends Scene
case class Cone(surface: ClosureValue) extends Scene
case class Plane(surface: ClosureValue) extends Scene

case class Union(left: Scene, right: Scene) extends Scene
case class Intersect(left: Scene, right: Scene) extends Scene
case class Difference(left: Scene, right: Scene) extends Scene

case class RotateX(scene: Scene, degrees: Double) extends Scene
case class RotateY(scene: Scene, degrees: Double) extends Scene
case class RotateZ(scene: Scene, degrees: Double) extends Scene
case class Translate(scene: Scene, dx: Double, dy: Double, dz: Double)
        extends Scene
case class Scale(scene: Scene, rx: Double, ry: Double, rz: Double)
        extends Scene
case class UScale(scene: Scene, r: Double) extends Scene
