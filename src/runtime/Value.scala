// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

import render.{Point, Light}

sealed abstract class Value {
  def toBoolean: Boolean = this match {
    case BoolValue(b) => b
    case _ => typeError("Boolean")
  }

  def toInt: Int = this match {
    case IntValue(n) => n
    case _ => typeError("Int")
  }

  def toReal: Double = this match {
    case RealValue(d) => d
    case _ => typeError("Real")
  }

  def toStr: String = this match {
    case StringValue(s) => s
    case _ => typeError("String")
  }

  def toArray: Array[Value] = this match {
    case ArrayValue(elements) => elements
    case _ => typeError("Array")
  }

  def toClosure: ClosureValue = this match {
    case c @ ClosureValue(_, _) => c
    case _ => typeError("Function")
  }

  def toPoint: Point = this match {
    case PointValue(p) => p
    case _ => typeError("Point")
  }

  def toLight: Light = this match {
    case LightValue(l) => l
    case _ => typeError("Light")
  }

  def toScene: Scene = this match {
    case SceneValue(s) => s
    case _ => typeError("Scene")
  }

  private def typeError(s: String): Nothing =
    throw new Exception("type error: expected "+ s +" found "+ this)
}

case class BoolValue(b: Boolean) extends Value {
  override def toString = b.toString
}

case class IntValue(n: Int) extends Value {
  override def toString = n.toString
}

case class RealValue(d: Double) extends Value {
  override def toString = d.toString
}

case class StringValue(s: String) extends Value {
  override def toString = "\""+ s +"\""
}

case class ArrayValue(elements: Array[Value]) extends Value {
  override def toString = "["+ elements.mkString(", ") +"]"
}

case class ClosureValue(address: Int, context: Array[Value]) extends Value {
  override def toString = "{&"+ address +", "+ context.mkString(", ") +"}"
}

case class PointValue(point: Point) extends Value {
  override def toString = point.toString
}

case class LightValue(light: Light) extends Value {
  override def toString = light.toString
}

case class SceneValue(scene: Scene) extends Value {
  override def toString = scene.toString
}
