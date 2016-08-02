// class to for bidirectional maps between strings and integers
// used to reffer to the integers as the strings e.g. 
// val test = new bimap("zero", "one", "two")
// test.getByName("zero")    // => 0
// test.getByValue(2)  // => "two"

/*  This file is part of cpu2.
    cpu2 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    cpu2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with cpu2.  If not, see http://www.gnu.org/licenses/.*/

package util

class bimap {
  // stores touples: (key: String, value: UInt)
  private val names = new collection.mutable.HashSet[(String, Integer)]()
  private var nextUnusedValue = 0

  // constructor
  def this(inits: String*) = {
    this()
    for (s <- inits) addName(s)
  }

  private def nameExists(name: String) =
    names.exists({case (s, v) => name == s})

  private def valueExists(value: Integer) =
    names.exists({case (s, v) => value == v})

  // private so that this class is mutable after the constructor has run
  private def addName(s: String) {
    if (nameExists(s))
      throw new IllegalArgumentException("Names are not unique")
    else {
      names.add((s, nextUnusedValue)) // we have checked this is unique so don't check return value
      nextUnusedValue += 1
    }
  }

  def byName(name: String): Integer =
    if (!nameExists(name))
      throw new IllegalArgumentException("Requested name \"" + name + "\" does not exist")
    else
      names.filter({case (s: String, v: Integer) => name == s}).head._2 // names are unique

  def byValue(value: Integer): String =
    if (!valueExists(value))
      throw new IllegalArgumentException("Requested value " + value + " does not exist")
    else
      names.filter({case (s: String, v: Integer) => value == v}).head._1 // values are unique
}


