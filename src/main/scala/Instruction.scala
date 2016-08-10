// Opcodes

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

object Instruction {
  val opcodes = new util.Bimap(
    "nop",              // do nothing
    "addImmediate",     // reg1 = (A) + (Imm)
    "subImmediate",     // reg1 = (A) - (Imm)
    "add",              // (dest) = (A) + (B)
    "sub",              // (ditto)..
    "nand",
    "lshift",
    "jumpToReg",        // PC = (A)
    "RESERVED_jumpRelative",
    "branchIfZero",     // if (ALU_ZERO) PC = (A)
    "branchIfPositive", // if (ALU_POSITIVE) PC = (A)
    "RESERVED_branchRelativeEqual",
    "RESERVED_branchRelativeLessThan",
    "RESERVED_loadImmediate",
    "load",             // (dest) = RAM(A)
    "store",            // RAM(A) = (B)
    "RESERVED_printBuffer",
    "halt"              // stop executing
  )

  // functionality for every Instruciton
  trait Instruction {
    def opcodeName: String
    def opcodeValue: Integer = Instruction.opcodes.byName(opcodeName)

    def encode: Integer

    def a: Option[Integer]
    def b: Option[Integer]
    def result: Option[Integer]
    def immediate: Option[Integer]

    override def toString(): String = {
      def argToString[T](arg: Option[T], first: Boolean): String = arg match {
        case Some(x) => if (first) x.toString else ", " + x.toString
        case None => new String()
      }

      opcodeName + "(" + argToString(a, true) + argToString(b, false) + argToString(result, false) + argToString(immediate, false) + ")"
    }

    protected def isRegister(i: Integer): Integer =
      if (!((i < 32) && (i >= 0)))
        throw new IllegalArgumentException(i.toString + " is not a valid register")
      else
        i

    protected def isImmediate(i: Integer): Integer =
      if (!(i < (1 << 21)) && (i >= -((1 << 21))))
        throw new IllegalArgumentException(i.toString + " is not a valid immediate value")
      else
        if (i < 0) i & 0x3FFFFFF else i // zero out the top 10 sign bits to reduce to 22 bits
  }

  // how to encode different Instruction types
  abstract class NoArgInstruction extends Instruction {
    def encode = opcodeValue

    def a = None
    def b = None
    def result = None
    def immediate = None
  }

  abstract class ImmediateInstruciton(_a: Integer, _immediate: Integer) extends Instruction {
    isRegister(_a)
    isImmediate(_immediate)

    def encode = opcodeValue | (_a << 5) | (_immediate << 10)

    def a = Some(_a)
    def b = None
    def result = None
    def immediate = Some(_immediate)
  }

  abstract class RegisterOperation(_a: Integer, _b: Integer, _result: Integer) extends Instruction {
    isRegister(_a)
    isRegister(_b)
    isRegister(_result)

    def encode = opcodeValue | (_a << 5) | (_b << 10) | (_result << 15)

    def a = Some(_a)
    def b = Some(_b)
    def result = Some(_result)
    def immediate = None
  }

  abstract class FlowControl(_a: Integer) extends Instruction {
    isRegister(_a)

    def encode = opcodeValue | (_a << 5)

    def a = Some(_a)
    def b = None
    def result = None
    def immediate = None
  }

  // actual instruction classes for things to use
  case object Nop extends NoArgInstruction {
    def opcodeName = "nop"
  }

  case class AddImmediate(_a: Integer, _imm: Integer) extends ImmediateInstruciton(_a, _imm) {
    def opcodeName = "addImmediate"
  }

  case class SubImmediate(_a: Integer, _imm: Integer) extends ImmediateInstruciton(_a, _imm) {
    def opcodeName = "subImmediate"
  }

  case class Add(_a: Integer, _b: Integer, _result: Integer) extends RegisterOperation(_a, _b, _result) {
    def opcodeName = "add"
  }

  case class Sub(_a: Integer, _b: Integer, _result: Integer) extends RegisterOperation(_a, _b, _result) {
    def opcodeName = "add"
  }

  case class Nand(_a: Integer, _b: Integer, _result: Integer) extends RegisterOperation(_a, _b, _result) {
    def opcodeName = "nand"
  }

  case class Lshift(_a: Integer, _b: Integer, _result: Integer) extends RegisterOperation(_a, _b, _result) {
    def opcodeName = "lshift"
  }

  case class JumpToReg(_a: Integer) extends FlowControl(_a) {
    def opcodeName = "jumpToReg"
  }

  case class BranchIfZero(_a: Integer) extends FlowControl(_a) {
    def opcodeName = "branchIfZero"
  }

  case class BranchIfPositive(_a: Integer) extends FlowControl(_a) {
    def opcodeName = "branchIfPositive"
  }

  case class Load(_a: Integer, _result: Integer) extends Instruction {
    isRegister(_a)
    isRegister(_result)

    def opcodeName = "load"

    def encode = opcodeValue | (_a << 5) | (_result << 15)

    def a = Some(_a)
    def b = None
    def result = Some(_result)
    def immediate = None
  }

  case class Store(_a: Integer, _b: Integer) extends Instruction {
    isRegister(_a)
    isRegister(_b)

    def opcodeName = "store"

    def encode = opcodeValue | (_a << 5) | (_b << 10)

    def a = Some(_a)
    def b = Some(_b)
    def result = None
    def immediate = None
  }

  case object Halt extends NoArgInstruction {
    def opcodeName = "halt"
  }

}

