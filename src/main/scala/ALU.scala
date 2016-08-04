// Arithmetic-Logic-Unit
// All combinatonal logic (large area but fast and simple)

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

object ALU {
  val controlOps = new util.Bimap("add", "sub", "nand", "lshift", "nop")

  val controlLineWidth = log2Up(controlOps.length)

  class io extends Bundle {
    val control = UInt(INPUT, controlLineWidth)
    val A = SInt(INPUT, 32) // A and B are the operands to the ALU operation
    val B = SInt(INPUT, 32)
    val result = SInt(OUTPUT, 32)
    val zeroFlag = Bool(OUTPUT)
    val positive = Bool(OUTPUT)
  }

  def main(args: Array[String]): Unit = {
   chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
          () => Module(new ALU)){c => new ALUTest(c)}
 }
}

class ALU extends Module {
  val io = new ALU.io
 
  io.result := UInt(0) // default value that should never happen
  switch(io.control) {
    is (UInt(ALU.controlOps.byName("add"))) {
      io.result := io.A + io.B
    }
    is (UInt(ALU.controlOps("sub"))) {
      io.result := io.A - io.B
    }
    is (UInt(ALU.controlOps("nand"))) {
      io.result := ~(io.A & io.B)
    }
    is (UInt(ALU.controlOps("lshift"))) {
      io.result := io.A.toBits << io.B.toBits
    }
    is (UInt(ALU.controlOps("nop"))) {
      io.result := UInt(0) // arbitary (logical "don't care")
    }
  }

  io.zeroFlag := io.result === SInt(0)
  io.positive := io.result >= SInt(0)
}

class ALUTest(dut: ALU) extends Tester(dut) {
  val testMax = 50
  for {
    A <- -testMax until testMax
    B <- -testMax until testMax
    op <- List("add", "sub", "nand", "nop")
  } test(A, B, op)

  // leftshift does weird things in scala for big numbers
  // so test separately here with nice numbers
  for {
    A <- 0 until 16
    B <- 0 until 15
  } test(A, B, "lshift")

  private def test(A: Integer, B: Integer, control: String): Unit = {
    val result = control match {
      case "add" => A + B
      case "sub" => A - B
      case "nand" => ~(A & B)
      case "lshift" => A << B
      case "nop" => 0
    }

    println("Testing " + A + " " + control + " " + B + " = " + result)

    poke(dut.io.A, A)
    poke(dut.io.B, B)
    poke(dut.io.control, ALU.controlOps.byName(control))

    step(1)

    expect(dut.io.result, result)
    expect(dut.io.zeroFlag, result == 0)
    expect(dut.io.positive, result >= 0)
  }
}
