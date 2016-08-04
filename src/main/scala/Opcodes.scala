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

object CPU {
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
}

