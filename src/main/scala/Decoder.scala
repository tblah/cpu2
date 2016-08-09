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

  // args is a list of touples containing the name of the element we are looking for and it's value
  // e.g. ("A", 1) :: ("B", 2) :: ("Result", 3) :: Nil
  // for usage examples see the tests for Decoder
  def encodeInstruction(opcode: String, args: (String, Integer)*): Integer = {
    def isRegister(i: Integer): Integer =
      if (!((i < 32) && (i >= 0)))
        throw new IllegalArgumentException(i.toString + " is not a valid register")
      else
        i

    def isImmediate(i: Integer): Integer =
      if (!(i < (1 << 21)) && (i >= -((1 << 21))))
        throw new IllegalArgumentException(i.toString + " is not a valid immediate value")
      else 
        if (i < 0) i & 0x3FFFFFF else i // zero out the top 10 sign bits to reduce to 22 bits

    def getArg(name: String): Integer =
      args.find({case (s, i) => s == name}).getOrElse(
        throw new IllegalArgumentException("Not found argument " + name))._2

    def a: Integer = isRegister(getArg("A")) << 5
    def b: Integer = isRegister(getArg("B")) << 10
    def result: Integer = isRegister(getArg("Result")) << 15
    def imm: Integer = isImmediate(getArg("Immediate")) << 10

    opcode match {
      case "nop" | "halt"                    => CPU.opcodes.byName(opcode)
      case "addImmediate" | "subImmediate"   => CPU.opcodes.byName(opcode) | a | imm
      case "add" | "sub" | "nand" | "lshift" => CPU.opcodes.byName(opcode) | a | b | result
      case "jumpToReg" | "branchIfZero" | "branchIfPositive" => CPU.opcodes.byName(opcode) | a
      case "load"                            => CPU.opcodes.byName(opcode) | a |     result
      case "store"                           => CPU.opcodes.byName(opcode) | a | b
      case _ => throw new IllegalArgumentException("Unrecognised opcode " + opcode)
    }
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
  val opcodes: Seq[String] = CPU.opcodes.listNames.filter((s: String) => !s.contains("RESERVED_"))

  val gen = new scala.util.Random()

  def regNum = gen.nextInt(32)
  def immediate = gen.nextInt((1<<22)-1) - (1<<21) + 1

  for (opcode <- opcodes; x <- 1 until testsPerOp) {
    opcode match {
      case "nop" | "halt" => {
        println("Testing " + opcode)
        poke(dut.io.memoryWord, Decoder.encodeInstruction(opcode))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
      }

      case "addImmediate" | "subImmediate" => {
        val a = regNum
        val imm = immediate

        println("Testing " + opcode + " " + a + " " + imm)

        poke(dut.io.memoryWord, Decoder.encodeInstruction(opcode, ("A", a), ("Immediate", imm)))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
        expect(dut.io.immediate, imm)
        expect(dut.io.A, a)
      }

      case "add" | "sub" | "nand" | "lshift" => {
        val a = regNum
        val b = regNum
        val res = regNum

        println("Testing " + opcode + " " + a + " " + b + " " + res)

        poke(dut.io.memoryWord,
          Decoder.encodeInstruction(opcode, ("A", a), ("B", b), ("Result", res)))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
        expect(dut.io.A, a)
        expect(dut.io.B, b)
        expect(dut.io.result, res)
      }

      case "jumpToReg" | "branchIfZero" | "branchIfPositive" => {
        val a = regNum

        println("Testing " + opcode + " " + a)

        poke(dut.io.memoryWord, Decoder.encodeInstruction(opcode, ("A", a)))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
        expect(dut.io.A, a)
      }

      case "load" => {
        val a = regNum
        val res = regNum

        println("Testing " + opcode + " " + a + " " + res)

        poke(dut.io.memoryWord, Decoder.encodeInstruction(opcode, ("A", a), ("Result", res)))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
        expect(dut.io.A, a)
        expect(dut.io.result, res)
      }

      case "store" => {
        val a = regNum
        val b = regNum

        println("Testing " + opcode + " " + a + " " + b)

        poke(dut.io.memoryWord, Decoder.encodeInstruction(opcode, ("A", a), ("B", b)))
        step(1)
        expect(dut.io.opcode, CPU.opcodes.byName(opcode))
        expect(dut.io.A, a)
        expect(dut.io.B, b)
      }

      case _ => println("WARNING!!!! NO TESTS FOR OPCODE " + opcode)
    }
  }
}


