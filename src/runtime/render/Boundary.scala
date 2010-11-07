// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime.render

case class Boundary(time: Double, shapePoint: Point, shape: Shape, face: Int) {
  def <(that: Boundary) = time < that.time

  def worldPoint = shape.view.pointToWorldSpace(shapePoint)

  def faceCoordinates = shape.faceCoordinates(face, shapePoint)
}

object Boundary {
  type Boundaries = List[Boundary]

  def union(as: Boundaries, bs: Boundaries): Boundaries =
    (as, bs) match {
      case (Nil, _) => bs
      case (_, Nil) => as
      case (a :: as0, b :: bs0) =>
        if (a < b)      a :: difference(as0, bs)
        else if (b < a) b :: difference(bs0, as)
        else            a :: intersect(as0, bs0)
    }

  def intersect(as: Boundaries, bs: Boundaries): Boundaries =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (a :: as0, b :: bs0) =>
        if (a < b)      difference(bs, as0)
        else if (b < a) difference(as, bs0)
        else            a :: union(as0, bs0)
    }

  def difference(as: Boundaries, bs: Boundaries): Boundaries =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => as
      case (a :: as0, b :: bs0) =>
        if (a < b)      a :: union(as0, bs)
        else if (b < a) intersect(as, bs0)
        else            difference(bs0, as0)
    }

  def firstEntry(as: Boundaries): Option[Boundary] =
    as match {
      case a :: _ if (a.time > 0) => Some(a)
      case _ :: _ :: a :: _ => Some(a)
      case _ => None
    }
}
