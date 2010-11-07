// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

import Math._
import render._

object Operators {
  type OpType = DataStack => Unit

  def apply(name: String): OpType =
    opTable(name)

  def isDefinedAt(name: String): Boolean =
    opTable contains name

  val opTable: Map[String, OpType] = Map(
    "acos"       -> liftR(x => RealValue(toDegrees(acos(x)))),
    "addf"       -> liftRR((x, y) => RealValue(x + y)),
    "addi"       -> liftII((x, y) => IntValue(x + y)),
    "asin"       -> liftR(x => RealValue(toDegrees(asin(x)))),
    "clampf"     -> liftR(clamp),
    "cos"        -> liftR(x => RealValue(cos(toRadians(x)))),
    "divf"       -> liftRR((x, y) => RealValue(x / y)),
    "divi"       -> liftII((x, y) => IntValue(x / y)),
    "eqf"        -> liftRR((x, y) => BoolValue(x == y)),
    "eqi"        -> liftII((x, y) => BoolValue(x == y)),
    "false"      -> { _ push(BoolValue(false)) },
    "floor"      -> liftR(x => IntValue(floor(x).toInt)),
    "frac"       -> liftR(frac),
    "get"        -> doGet,
    "length"     -> doLength,
    "lessf"      -> liftRR((x, y) => BoolValue(x < y)),
    "lessi"      -> liftII((x, y) => BoolValue(x < y)),
    "modi"       -> liftII((x, y) => IntValue(x % y)),
    "mulf"       -> liftRR((x, y) => RealValue(x * y)),
    "muli"       -> liftII((x, y) => IntValue(x * y)),
    "negf"       -> liftR(x => RealValue(-x)),
    "negi"       -> liftI(x => IntValue(-x)),
    "real"       -> liftI(x => RealValue(x)),
    "sin"        -> liftR(x => RealValue(sin(toRadians(x)))),
    "sqrt"       -> liftR(x => RealValue(sqrt(x))),
    "subf"       -> liftRR((x, y) => RealValue(x - y)),
    "subi"       -> liftII((x, y) => IntValue(x - y)),
    "true"       -> { _ push(BoolValue(true)) },

    "cone"       -> liftC(s => SceneValue(Cone(s))),
    "cube"       -> liftC(s => SceneValue(Cube(s))),
    "cylinder"   -> liftC(s => SceneValue(Cylinder(s))),
    "difference" -> liftSS((l, r) => SceneValue(Difference(l, r))),
    "getx"       -> liftP(p => RealValue(p.x)),
    "gety"       -> liftP(p => RealValue(p.y)),
    "getz"       -> liftP(p => RealValue(p.z)),
    "intersect"  -> liftSS((l, r) => SceneValue(Intersect(l, r))),
    "light"      -> liftPP((d, c) => LightValue(PlainLight(d, c))),
    "plane"      -> liftC(s => SceneValue(Plane(s))),
    "point"      -> liftRRR((x, y, z) => PointValue(Point(x, y, z))),
    "pointlight" -> liftPP((p, c) => LightValue(PointLight(p, c))),
    "rotatex"    -> liftSR((s, d) => SceneValue(RotateX(s, d))),
    "rotatey"    -> liftSR((s, d) => SceneValue(RotateY(s, d))),
    "rotatez"    -> liftSR((s, d) => SceneValue(RotateZ(s, d))),
    "scale"      -> liftSRRR((s, x, y, z) => SceneValue(Scale(s, x, y, z))),
    "sphere"     -> liftC(s => SceneValue(Sphere(s))),
    "spotlight"  -> doSpotLight,
    "translate"  -> liftSRRR((s, x, y, z) => SceneValue(Translate(s, x, y, z))),
    "union"      -> liftSS((l, r) => SceneValue(Union(l, r))),
    "uscale"     -> liftSR((s, r) => SceneValue(UScale(s, r)))
  )

  def liftR(f: Double => Value)(data: DataStack): Unit =
    data.push(f(data.pop.toReal))

  def liftI(f: Int => Value)(data: DataStack): Unit =
    data.push(f(data.pop.toInt))

  def liftC(f: ClosureValue => Value)(data: DataStack): Unit =
    data.push(f(data.pop.toClosure))

  def liftP(f: Point => Value)(data: DataStack): Unit =
    data.push(f(data.pop.toPoint))

  def liftRR(f: (Double, Double) => Value)(data: DataStack) = {
    val arg2 = data.pop.toReal
    val arg1 = data.pop.toReal
    data.push(f(arg1, arg2))
  }

  def liftII(f: (Int, Int) => Value)(data: DataStack): Unit = {
    val arg2 = data.pop.toInt
    val arg1 = data.pop.toInt
    data.push(f(arg1, arg2))
  }

  def liftSS(f: (Scene, Scene) => Value)(data: DataStack): Unit = {
    val r = data.pop.toScene
    val l = data.pop.toScene
    data.push(f(l, r))
  }

  def liftSR(f: (Scene, Double) => Value)(data: DataStack): Unit = {
    val r = data.pop.toReal
    val s = data.pop.toScene
    data.push(f(s, r))
  }

  def liftPP(f: (Point, Point) => Value)(data: DataStack): Unit = {
    val p2 = data.pop.toPoint
    val p1 = data.pop.toPoint
    data.push(f(p1, p2))
  }

  def liftRRR(f: (Double, Double, Double) => Value)(data: DataStack): Unit = {
    val z = data.pop.toReal
    val y = data.pop.toReal
    val x = data.pop.toReal
    data.push(f(x, y, z))
  }

  def liftSRRR(f: (Scene, Double, Double, Double) => Value)
              (data: DataStack): Unit =
  {
    val z = data.pop.toReal
    val y = data.pop.toReal
    val x = data.pop.toReal
    val s = data.pop.toScene
    data.push(f(s, x, y, z))
  }

  private def clamp(d: Double): Value =
    RealValue(if (d < 0) 0 else if (d > 1) 1 else d)

  private def frac(d: Double): Value =
    RealValue(d - (if (d < 0) ceil(d) else floor(d)))

  private def doGet(data: DataStack): Unit = {
    val i = data.pop.toInt
    val a = data.pop.toArray
    data.push(a(i))
  }

  private def doLength(data: DataStack): Unit =
    data.push(IntValue(data.pop.toArray.length))

  private def doSpotLight(data: DataStack): Unit = {
    val exponent = data.pop.toReal
    val cutoff = data.pop.toReal
    val color = data.pop.toPoint
    val at = data.pop.toPoint
    val position = data.pop.toPoint
    data.push(LightValue(SpotLight(position, at, color, cutoff, exponent)))
  }
}
