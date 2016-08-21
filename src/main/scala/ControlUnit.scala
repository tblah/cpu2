// Control Unit
// ties everything together: implementing the datapaths and the control unit
// 32 bit words

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

object ControlUnit {
  val state = new util.Bimap("fetch", "decode", "execute", "write")

  class io extends Bundle {
    val ram = (new RAM.io(4)).flip // 32 bit words
    val regFile = (new RegisterFile.io(5, 32)).flip // 5 bit address, 32 bit words
    val alu = (new ALU.io).flip
    val decoder = (new Decoder.io).flip
  }

  def test: Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new ControlUnit))
    {c => new ControlUnitTest(c)}
  }
}

class ControlUnit extends Module {
  val io = new ControlUnit.io

  // special-purpouse registers
  val programCounter = Reg(init=UInt(0, width=32))
  val pcPlus4 = Reg(outType = UInt(width = 32))
  val resultArg = Reg(outType = UInt(width = 5))
  val immediate = Reg(outType = SInt(width = 22))
  val aluResult = Reg(outType = SInt(width = 32))
  val aluZero = Reg(outType = Bool())
  val aluPositive = Reg(outType = Bool())
  val halted = Reg(outType = Bool(), init = Bool(false))
  val state = Reg(outType = UInt(width = log2Up(ControlUnit.state.length)),
    init = UInt(ControlUnit.state.byName("fetch")))
  val currentOpcode = Reg(outType = UInt(width = 5))

  // multiplexers (for conditional branches)
  val ifZeroMux = Mux(aluZero, io.regFile.read0.data, pcPlus4)
  val ifPositiveMux = Mux(aluPositive, io.regFile.read0.data, pcPlus4)

  // default values for outputs
  io.decoder.memoryWord := io.ram.dataOut
  io.regFile.writeEnable := Bool(false)
  io.regFile.read0.address := io.decoder.A
  io.regFile.read1.address := io.decoder.B
  io.regFile.write.address := resultArg
  io.regFile.write.data := aluResult
  io.ram.address := io.regFile.read0.data
  io.ram.writeEnable := Bool(false)
  io.ram.dataIn := io.regFile.read1.data
  io.alu.control := UInt(ALU.controlOps.byName("nop"))
  io.alu.A := io.regFile.read0.data
  io.alu.B := io.regFile.read1.data

  // handle each control state
  private def fetch(): Unit = {
    // check to see if we are supposed to be halted
    when (halted) {
      state := UInt(ControlUnit.state.byName("fetch"))
      return
    }

    // read next instruction in from RAM
    io.ram.address := programCounter

    state := UInt(ControlUnit.state.byName("decode"))
  }

  private def decode(): Unit = {
    // decode the word we just read from RAM
    // most of this wiring is done by the default values
    currentOpcode := io.decoder.opcode
    resultArg := io.decoder.result
    immediate := io.decoder.immediate

    // calculate pcPlus4
    io.alu.control := UInt(ALU.controlOps.byName("add"))
    io.alu.A := programCounter
    io.alu.B := UInt(4)
    pcPlus4 := io.alu.result

    state := UInt(ControlUnit.state.byName("execute"))
  }

  private def aluExecute(aluOp: String, imm: Boolean): Unit = {
    io.alu.control := UInt(ALU.controlOps.byName(aluOp))

    if (imm)
      io.alu.B := immediate

    aluResult := io.alu.result
    aluZero := io.alu.zeroFlag
    aluPositive := io.alu.positive

    state := UInt(ControlUnit.state.byName("write"))
  }

  private def branchExecute(pc: UInt): Unit = {
    programCounter := pc
    state := UInt(ControlUnit.state.byName("fetch"))
  }

  private def execute(): Unit = {
    // default new pc value
    programCounter := pcPlus4

    // actually execute instructions
    switch (currentOpcode) {
      is (UInt(Instruction.opcodes.byName("add"))) {
        aluExecute("add", false)
      }

      is (UInt(Instruction.opcodes.byName("sub"))) {
        aluExecute("sub", false)
      }

      is (UInt(Instruction.opcodes.byName("nand"))) {
        aluExecute("nand", false)
      }

      is (UInt(Instruction.opcodes.byName("lshift"))) {
        aluExecute("lshift", false)
      }

      is (UInt(Instruction.opcodes.byName("addImmediate"))) {
        aluExecute("add", true)
      }

      is (UInt(Instruction.opcodes.byName("subImmediate"))) {
        aluExecute("sub", true)
      }

      is (UInt(Instruction.opcodes.byName("jumpToReg"))) {
        branchExecute(io.regFile.read0.data)
      }

      is (UInt(Instruction.opcodes.byName("branchIfZero"))) {
        branchExecute(ifZeroMux)
      }

      is (UInt(Instruction.opcodes.byName("branchIfPositive"))) {
        branchExecute(ifPositiveMux)
      }

      is (UInt(Instruction.opcodes.byName("load"))) {
        state := UInt(ControlUnit.state.byName("write"))
      }

      is (UInt(Instruction.opcodes.byName("store"))) {
        state := UInt(ControlUnit.state.byName("fetch"))
        io.ram.writeEnable := Bool(true)
      }

      is (UInt(Instruction.opcodes.byName("nop"))) {
        state := UInt(ControlUnit.state.byName("fetch"))
      }

      is (UInt(Instruction.opcodes.byName("halt"))) {
        halted := Bool(true)
        state := UInt(ControlUnit.state.byName("fetch"))
      }
    }
  }


  private def aluWrite(imm: Boolean): Unit = {
    io.regFile.write.data := aluResult

    if (imm)
      io.regFile.write.address := UInt(0)
    else
      io.regFile.write.address := resultArg
  }

  private def write(): Unit= {
    state := UInt(ControlUnit.state.byName("fetch"))
    io.regFile.writeEnable := Bool(true)

    switch (currentOpcode) {
      is (UInt(Instruction.opcodes.byName("add"))) {
        aluWrite(false)
      }

      is (UInt(Instruction.opcodes.byName("sub"))) {
        aluWrite(false)
      }

      is (UInt(Instruction.opcodes.byName("nand"))) {
        aluWrite(false)
      }

      is (UInt(Instruction.opcodes.byName("lshift"))) {
        aluWrite(false)
      }

      is (UInt(Instruction.opcodes.byName("addImmediate"))) {
        aluWrite(true)
      }

      is (UInt(Instruction.opcodes.byName("subImmediate"))) {
        aluWrite(true)
      }

      is (UInt(Instruction.opcodes.byName("load"))) {
        io.regFile.write.address := resultArg
        io.regFile.write.data := io.ram.dataOut
      }
    }
  }

  // fsm
  switch(state) {
    is (UInt(ControlUnit.state.byName("fetch"))) {
      fetch()
    }
    is (UInt(ControlUnit.state.byName("decode"))) {
      decode()
    }
    is (UInt(ControlUnit.state.byName("execute"))) {
      execute()
    }
    is (UInt(ControlUnit.state.byName("write"))) {
      write()
    }
  }
}

class ControlUnitTest(dut: ControlUnit) extends Tester(dut) {

}

