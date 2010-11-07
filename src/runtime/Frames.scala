// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

class Frames {
  var frames = new Array[Value](4096)
  var framePtr = -1

  def enter(size: Int): Unit = {
    framePtr += size
    if (framePtr >= frames.length) frames = ExpandArray.resize(frames, framePtr)
  }

  def leave(size: Int): Unit = {
    framePtr -= size
    if (framePtr < -1) throw new Exception("gml: frame stack underflow")
  }

  def apply(slot: Int): Value =
    frames(framePtr - slot)

  def update(slot: Int, value: Value): Unit =
    frames(framePtr - slot) = value
}
