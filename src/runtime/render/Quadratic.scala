// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

object Quadratic {
  sealed abstract class Roots
  case object NoRoots extends Roots
  case class OneRoot(root: Double) extends Roots
  case class DoubleRoot(root: Double) extends Roots
  case class TwoRoots(low: Double, high: Double) extends Roots

  // Return roots of a*t*t + 2*b*t + c.
  def solve(a: Double, b: Double, c: Double): Roots =
    if (a < 0) solvePos(-a, -b, -c)
    else if (a > 0) solvePos(a, b, c)
    else if (b != 0) OneRoot(-c / (2 * b))
    else NoRoots

  // As above but assumes a > 0.  Method to reduce rounding error
  // adapted from "Numerical recipes".
  def solvePos(a: Double, b: Double, c: Double): Roots = {
    val dSqr = b*b - a*c
    if (dSqr < 0) NoRoots
    else if (dSqr == 0) DoubleRoot(-b / a)
    else {
      val d = Math.sqrt(dSqr)
      if (b < 0) {
        val q = d - b
        TwoRoots(c / q, q / a)
      } else if (b > 0) {
        val q = -(d + b)
        TwoRoots(q / a, c / q)
      } else {
        val q = d / a
        TwoRoots(-q, q)
      }
    }
  }
}
