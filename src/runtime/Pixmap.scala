// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

import render.Point
import java.io.{BufferedOutputStream, FileOutputStream}

class Pixmap(width: Int, height: Int) {
  val header = "P6 "+ width +" "+ height +" 255\n"

  val pixels = new Array[Array[Point]](height, width)

  def update(x: Int, y: Int, color: Point): Unit = {
    pixels(y)(x) = color
  }

  def output(filename: String): Unit = {
    val s = new BufferedOutputStream(new FileOutputStream(filename))
    s.write(header.getBytes)
    for (row <- pixels; pixel <- row) {
      s.write(colorVal(pixel.x))
      s.write(colorVal(pixel.y))
      s.write(colorVal(pixel.z))
    }
    s.close
  }

  // Convert from 0..1 to 0..255.
  def colorVal(v: Double): Int = {
    val scaled = v * 256
    if (scaled < 0) 0
    else if (scaled > 255) 255
    else scaled.toInt
  }
}
