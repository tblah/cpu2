// Addressed access to the general purpuse registers
// two read channels, one write channel
// cannot read and write in the same clock cycle

// uses sequential memory

// the register at address 0 always reads 0

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

object RegisterFile {
  class RegisterChannel(dataDir: IODirection, addressWidth: Integer, wordWidth: Integer)
      extends Bundle {
    assume(addressWidth > 0)
    assume(wordWidth > 0)

    val address = UInt(INPUT, width = addressWidth)
    val data = Bits(dataDir, width = wordWidth)
  }

  class io(addressWidth: Integer, wordWidth: Integer) extends Bundle {
    assume(addressWidth > 0)
    assume(wordWidth > 0)

    val writeEnable = Bool(INPUT)
    val read0 = new RegisterChannel(OUTPUT, addressWidth, wordWidth)
    val read1 = new RegisterChannel(OUTPUT, addressWidth, wordWidth)
    val write = new RegisterChannel(INPUT, addressWidth, wordWidth)
  }

  def main(args: Array[String]): Unit = {
    val numRegisters = 32
    val wordWidth = 32
    
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new RegisterFile(numRegisters, wordWidth)))
    {c => new RegisterFileTest(c)}
  }
}

class RegisterFile(val numRegisters: Integer, val wordWidth: Integer) extends Module {
  assume(numRegisters > 0)
  assume(wordWidth > 0)

  val addressWidth = log2Up(numRegisters)
  val io = new RegisterFile.io(addressWidth, wordWidth)

  val mem = Mem(n = numRegisters, seqRead = true, out = Bits(width=wordWidth))

  // default values
  io.read0.data := Bits(0)
  io.read1.data := Bits(0)

  when (io.writeEnable) {
    mem(io.write.address) := io.write.data
  } .otherwise {
    doRead(io.read0)
    doRead(io.read1)
  }

  def doRead(reader: RegisterFile.RegisterChannel) = {
    // make sure we always read register 0 as containing 0
    when (reader.address === UInt(0)) {
      reader.data := Bits(0)
    } .otherwise {
      reader.data := mem(reader.address)
    }
  }
}

class RegisterFileTest(dut: RegisterFile) extends Tester(dut) {
  private val gen = new scala.util.Random()

  private def genAddr = gen.nextInt(dut.numRegisters)
  private def genData = gen.nextInt()

  private def runTest(addr0: Integer = genAddr,
    addr1: Integer = genAddr,
    data0: Integer = genData,
    data1: Integer = genData) = {

    @scala.annotation.tailrec
    def checkAddrs(a1: Integer, a2: Integer): Integer = if (a1 == a2) checkAddrs(a1, genAddr) else a2

    def write(addr: Integer, data: Integer) = {
      println("Writing " + data + " to " + addr)
      poke(dut.io.writeEnable, true)
      poke(dut.io.write.address, addr)
      poke(dut.io.write.data, data)
      step(1)
    }

    def read(reader: RegisterFile.RegisterChannel, addr: Integer, data: Integer) = {
      poke(dut.io.writeEnable, false)
      poke(reader.address, addr)
    }

    def readExpect(reader: RegisterFile.RegisterChannel, addr: Integer, data: Integer) = {
      // reads to address 0 always return 0
      if (addr == 0)
        expect(reader.data, 0)
      else
        expect(reader.data, data)
    }

    // don't read and write from the same place simaltaniously within the same test
    val addr1_fixed = checkAddrs(addr0, addr1)

    // do writes
    write(addr0, data0)
    write(addr1_fixed, data1)
   
    // read back both in the same clock cycle
    println("Reading data back on channel 0")
    read(dut.io.read0, addr0, data0)

    println("Reading data back on channel 1")
    read(dut.io.read1, addr1_fixed, data1)

    step(1)

    readExpect(dut.io.read0, addr0, data0)
    readExpect(dut.io.read1, addr1_fixed, data1)

  }

  val numTests = 100

  for (i <- 1 until numTests) {
    runTest()
  }

  // make sure we have tested the case of address 0
  runTest(0, 10)
}





  
