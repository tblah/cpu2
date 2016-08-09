// Run all tests

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

import cpu._

object Tests {
  def runTest(name: String, test: => Unit): Unit = {
    print("Do you wan't to run the test suite for " + name + "? (y/n) ") 
    val c = scala.Console.readChar()
    c match {
      case 'Y' | 'y' => test
      case 'N' | 'n' => return
      case _ => runTest(name, test)
    }
  }

  def main(args: Array[String]): Unit = {
    runTest("ALU", ALU.test)
    runTest("Decoder", Decoder.test)
    runTest("RegisterFile", RegisterFile.test)
    runTest("RAM", RAM.test)
  }
}

