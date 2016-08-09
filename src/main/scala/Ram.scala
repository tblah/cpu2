// Model of an interface with some RAM
// the RAM is a lot faster here than it would be in a real system
// TODO: model slower RAM + cache hierarchy

// The addres is one word wide and the RAM is addressed in bytes

// everything assumes 4 byte words

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

object RAM {
  // address width is the same as the word width
  class io extends Bundle {
    val address = UInt(INPUT, 32)
    val writeEnable = Bool(INPUT)

    // chisel does not have bidirectonal io
    val dataIn = Bits(INPUT, 32)
    val dataOut = Bits(OUTPUT, 32)
  }

  def main(args: Array[String]): Unit = {
    val numWords = 1024
    
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new RAM(numWords)))
    {c => new RAMTest(c)}
  }
}

// initial data is an optional List of touples of (data, address) to initialise the ram with
// data should be 8 bits
class RAM(val numWords: Integer, initialData: Seq[(Bits, UInt)] = Seq()) extends Module {
  def bytesToWord(byte0: Bits, byte1: Bits, byte2: Bits, byte3: Bits): Bits =
    Cat(byte3, byte2, byte1, byte0) // endian-ness

  def wordToBytes(word: Bits): (Bits, Bits, Bits, Bits) =
    (word(7, 0), word(15, 8), word(23, 16), word(31, 24))

  val numBytes = 4 * numWords
  val maxAddr = numBytes - 3
  assume(log2Up(numBytes) <= 32, """RAM addresses are one word wide.
    This is not sufficient to index the number of bytes which you requested""")

  val io = new RAM.io

  val mem = Mem(Bits(width=8), numBytes, seqRead=true)

  // deal with the initial data
  for ((data, address) <- initialData) {
    assert(address < UInt(maxAddr))

    mem(address) := data
  }

  // check the address is sane
  assert(io.address < UInt(maxAddr), "Illegal address (too high)")

  // default output value
  io.dataOut := Bits(0)

  when (io.writeEnable) {
    val (byte0, byte1, byte2, byte3) = wordToBytes(io.dataIn)
    mem(io.address) := byte0
    mem(io.address + UInt(1)) := byte1
    mem(io.address + UInt(2)) := byte2
    mem(io.address + UInt(3)) := byte3
  } .otherwise {
    io.dataOut := bytesToWord(mem(io.address), mem(io.address + UInt(1)),
      mem(io.address + UInt(2)), mem(io.address + UInt(3)))
  }
}

class RAMTest(dut: RAM) extends Tester(dut) {
  private val gen = new scala.util.Random()

  private def genAddr = gen.nextInt(dut.maxAddr)
  private def genData = gen.nextInt()

  private def runTest: Unit = {
    val data = genData
    val address = genAddr

    println("Writing " + data + "to address " + address)
    poke(dut.io.address, address)
    poke(dut.io.writeEnable, true)
    poke(dut.io.dataIn, data)
    step(1)

    println("Reading from address " + address)
    poke(dut.io.address, address)
    poke(dut.io.writeEnable, false)
    step(1)
    expect(dut.io.dataOut, data)
  }

  val numTests = 100
  for (i <- 1 until numTests)
    runTest
}
