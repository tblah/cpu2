// Encapsulating module
// just wires and a test-suite

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

package cpu

import Chisel._

object CPU {
  def test: Unit = {
    val ramSize = 1024
    val initialData = List()

    /* we have to turn off combinational loop checking here because chisel finds some 95-stage loop mostly
       involving the register file and the wordToBytes and bytesToWord functions in RAM 
       I cannot see any loop myself so I have added --noCombLoop to turn it off and hope that nothing goes
       badly wrong. The C++ compiles okay and a look through the chisel github repo issues gives the impression
       that the generated C++ would be wonkey if there was a real combinational loop */
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--noCombLoop"),
      () => Module(new CPU(ramSize, initialData))){c => new CPUTest(c)}
  }
}

// see RAM.scala for description of initialData
class CPU(ramSize: Integer, initialData: Seq[(Bits, UInt)]) extends Module {
  val io = new Bundle // no I/O

  val ram = Module(new RAM(4, ramSize, initialData))
  val alu = Module(new ALU)
  val ctrl = Module(new ControlUnit)
  val regFile = Module(new RegisterFile(32, 32))
  val decoder = Module(new Decoder)

  ctrl.io.ram <> ram.io
  ctrl.io.regFile <> regFile.io
  ctrl.io.alu <> alu.io
  ctrl.io.decoder <> decoder.io
}

class CPUTest(dut: CPU) extends Tester(dut) {
}
