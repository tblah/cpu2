// Instruction decoder
// just does wiring from a memory word to the different bit-fields

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

object Decoder {
  class io extends Bundle {
    val memoryWord = Bits(INPUT, 32)
    val opcode = UInt(OUTPUT, 5)
    val A = UInt(OUTPUT, 5)
    val B = UInt(OUTPUT, 5)
    val result = UInt(OUTPUT, 5)
    val immediate = SInt(OUTPUT, 22)
  }

  def test: Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new Decoder)){c => new DecoderTest(c)}
  }
}

class Decoder extends Module {
  val io = new Decoder.io

  // opcode is the first 5 bits
  io.opcode := UInt().fromBits(io.memoryWord(4, 0))

  // most opcodes use the next 5 bits as the first register index
  io.A := UInt().fromBits(io.memoryWord(9, 5))

  // register-to-register instructions and store use the next 5 bits for the other register index
  io.B := UInt().fromBits(io.memoryWord(14,10))

  // register-to-register instructions and load use this 5-bit feild to store the register index of the result of the operation
  io.result := UInt().fromBits(io.memoryWord(19, 15))

  // imediate-value instructions use the last 22 bits for the immedaite value
  io.immediate := SInt().fromBits(io.memoryWord(31, 10))
}

class DecoderTest(dut: Decoder) extends Tester(dut) {
  val testsPerOp = 32 // number of times to run each test per opcode
  val opcodes: Seq[String] = Instruction.opcodes.listNames.filter((s: String) => !s.contains("RESERVED_"))

  val gen = new scala.util.Random()

  def genReg = gen.nextInt(32)
  def genImm = gen.nextInt((1<<22)-1) - (1<<21) + 1

  def test(inst: Instruction.Instruction) {
    def expectOption(port: Bits, x: Option[Integer]): Unit = x match {
      case Some(i) => expect(port, i)
      case None =>
    }

    println("Testing " + inst)

    poke(dut.io.memoryWord, inst.encode)
    step(1)

    expect(dut.io.opcode, inst.opcodeValue)
    expectOption(dut.io.A, inst.a)
    expectOption(dut.io.B, inst.b)
    expectOption(dut.io.result, inst.result)
    expectOption(dut.io.immediate, inst.immediate)
  }


  for (opcode <- opcodes; x <- 1 until testsPerOp) {
    opcode match {
      case "nop"              => test(Instruction.Nop) 
      case "addImmediate"     => test(Instruction.AddImmediate(genReg, genImm))
      case "subImmediate"     => test(Instruction.SubImmediate(genReg, genImm))
      case "add"              => test(Instruction.Add(genReg, genReg, genReg))
      case "sub"              => test(Instruction.Sub(genReg, genReg, genReg))
      case "nand"             => test(Instruction.Nand(genReg, genReg, genReg))
      case "lshift"           => test(Instruction.Lshift(genReg, genReg, genReg))
      case "jumpToReg"        => test(Instruction.JumpToReg(genReg))
      case "branchIfZero"     => test(Instruction.BranchIfZero(genReg))
      case "branchIfPositive" => test(Instruction.BranchIfPositive(genReg))
      case "load"             => test(Instruction.Load(genReg, genReg))
      case "store"            => test(Instruction.Store(genReg, genReg))
      case "halt"             => test(Instruction.Halt)
      case s                  => throw new Exception("Untested opcode " + s)
    }
  }
}


