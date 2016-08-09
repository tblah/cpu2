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
  class io (bytesPerWord: Integer) extends Bundle {
    assume(bytesPerWord > 0)
    val wordWidth = bytesPerWord * 8

    val address = UInt(INPUT, wordWidth)
    val writeEnable = Bool(INPUT)

    // chisel does not have bidirectonal io
    val dataIn = Bits(INPUT, wordWidth)
    val dataOut = Bits(OUTPUT, wordWidth)
  }

  def main(args: Array[String]): Unit = {
    val bytesPerWord = 4 // 32 bit word
    val numWords = 1024
    
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new RAM(bytesPerWord, numWords)))
    {c => new RAMTest(c)}
  }
}

// initial data is an optional List of touples of (data, address) to initialise the ram with
// data should be 8 bits
class RAM(val bytesPerWord: Integer, val numWords: Integer, initialData: Seq[(Bits, UInt)] = Seq()) extends Module {
  assume(bytesPerWord > 0)
  val numBytes = bytesPerWord * numWords
  val maxAddr = numBytes - (bytesPerWord - 1)
  assume(log2Up(numBytes) <= (bytesPerWord * 8), """RAM addresses are one word wide.
    This is not sufficient to index the number of bytes which you requested""")

  def bytesToWord(bytes: Vec[Bits]): Bits = {
    var result = bytes(bytesPerWord-1)
    for (i <- (bytesPerWord - 2) to 0 by -1) {  // going backwards because of endian-ness
      result = Cat(result, bytes(i))
    }

    result
  }

  def wordToBytes(word: Bits): Vec[Bits] = Vec(
    for {
      i <- 0 until bytesPerWord
      x <- word((8 * (i + 1)) - 1, 8 * i)
    } yield x)

  val io = new RAM.io(bytesPerWord)

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
    val byteVec = wordToBytes(io.dataIn)
    for (i <- 0 until bytesPerWord)
      mem(io.address + UInt(i)) := byteVec(i)

  } .otherwise {
    val bytes = for {
      i <- 0 until bytesPerWord
      x <- mem(io.address + UInt(i))
    } yield x

    io.dataOut := bytesToWord(Vec(bytes))
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
