// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package runtime

object ExpandArray {
  // Set the value at the index, resizing if needed.
  def set[T](array: Array[T], index: Int, value: T): Array[T] = {
    val newArray = if (index >= array.length) resize(array, index) else array
    newArray(index) = value
    newArray
  }

  // Double the array size until it includes the index.
  def resize[T](array: Array[T], index: Int): Array[T] = {
    var size = array.length
    if (size == 0) size = 1
    while (size <= index) size *= 2
    val newArray = new Array[T](size)
    array.copyToArray(newArray, 0)
    newArray
  }
}
