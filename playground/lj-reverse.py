#!/usr/bin/env python3

import sys
import struct
import math

# Constants
# Internal (LEB128 buffer)
MAX_ULEB_SIZE = 256 # Maximum proto size here is 2^7^256

# Reference: https://github.com/LuaJit/LuaJIT/bloc/master/src/lj_bc.h
class ByteCode:
    OP_VAR = 1
    OP_STR = 2
    OP_NUM = 3
    OP_PRI = 4
    OP_DST = 5
    OP_RBASE = 6
    OP_BASE = 7
    OP_CDATA = 8
    OP_LIT = 9 # Literal
    OP_LITS = 10 # Signed literal
    OP_FUNC = 11
    OP_UV = 12
    OP_JUMP = 13
    OP_TAB = 14
    OP_MNUM = 15 # Multiple nums
    OP_NIL = 16 # Always nil
    BC_MODE = 0
    AD_MODE = 1
    OPCODE_TABLE = {
            0x00: {"op": "ISLT", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}<{D}"},
            0x01: {"op": "ISGE", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}>={D}"},
            0x02: {"op": "ISLE", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}<={D}"},
            0x03: {"op": "ISGT", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}>{D}"},
            0x04: {"op": "ISEQV", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}={D}"},
            0x05: {"op": "ISNEV", "A": OP_VAR, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {A}!={D}"},
            0x06: {"op": "ISEQS", "A": OP_VAR, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "JMP if {A}=(STR)D"}, # For STR constants?
            0x07: {"op": "ISNES", "A": OP_VAR, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "JMP if {A}!=(STR)D"},
            0x08: {"op": "ISEQN", "A": OP_VAR, "B": None, "CD": OP_NUM, "mode": AD_MODE, "desc": "JMP if {A}=(NUM)D"}, # For NUM constants?
            0x09: {"op": "ISNEN", "A": OP_VAR, "B": None, "CD": OP_NUM, "mode": AD_MODE, "desc": "JMP if {A}!=(NUM)D"},
            0x0A: {"op": "ISEQP", "A": OP_VAR, "B": None, "CD": OP_PRI, "mode": AD_MODE, "desc": "JMP if {A}=D (primitive 0=nil,1=false,2=true)"},
            0x0B: {"op": "ISNEP", "A": OP_VAR, "B": None, "CD": OP_PRI, "mode": AD_MODE, "desc": "JMP if {A}!=D (primitive 0=nil,1=false,2=true)"},
            0x0C: {"op": "ISTC", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A}<=copy={D} then JMP if {D}=true"},
            0x0D: {"op": "ISFC", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A}<=copy={D} then JMP if {D}=false"},
            0x0E: {"op": "IST", "A": None, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {D}=true"},
            0x0F: {"op": "ISF", "A": None, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "JMP if {D}=false"},
            0x10: {"op": "MOV", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A}<=copy={D}"},
            0x11: {"op": "NOT", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A} <= NOT {D}"},
            0x12: {"op": "UNM", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A} <= -{D} (unary minus)"},
            0x13: {"op": "LEN", "A": OP_DST, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "{A} <= len({D})"},
            0x14: {"op": "ADDVN", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= {B} + (NUM)C"},
            0x15: {"op": "SUBVN", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= {B} - (NUM)C"},
            0x16: {"op": "MULVN", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= {B} * (NUM)C"},
            0x17: {"op": "DIVVN", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= {B} / (NUM)C"},
            0x18: {"op": "MODVN", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= {B} % (NUM)C"},
            0x19: {"op": "ADDNV", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= (NUM)C + {B}"},
            0x1A: {"op": "SUBNV", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= (NUM)C - {B}"},
            0x1B: {"op": "MULNV", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= (NUM)C * {B}"},
            0x1C: {"op": "DIVNV", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= (NUM)C / {B}"},
            0x1D: {"op": "MODNV", "A": OP_DST, "B": OP_VAR, "CD": OP_NUM, "mode": BC_MODE, "desc": "{A} <= (NUM)C % {B}"},
            0x1E: {"op": "ADDVV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} + {C}"},
            0x1F: {"op": "SUBVV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} - {C}"},
            0x20: {"op": "MULVV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} * {C}"},
            0x21: {"op": "DIVVV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} / {C}"},
            0x22: {"op": "MODVV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} % {C}"},
            0x23: {"op": "POW", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B} ^ {C}"},
            0x24: {"op": "CAT", "A": OP_DST, "B": OP_RBASE, "CD": OP_RBASE, "mode": BC_MODE, "desc": "{A} <= {B} ~ {B+1} ~ ... ~ {C}"},
            0x25: {"op": "KSTR", "A": OP_DST, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "{A} <= (STR)D"},
            0x26: {"op": "KCDATA", "A": OP_DST, "B": None, "CD": OP_CDATA, "mode": AD_MODE, "desc": "{A} <= (CDATA)D"},
            0x27: {"op": "KSHORT", "A": OP_DST, "B": None, "CD": OP_LITS, "mode": AD_MODE, "desc": "{A} <= D (16-bit signed int)"},
            0x28: {"op": "KNUM", "A": OP_DST, "B": None, "CD": OP_NUM, "mode": AD_MODE, "desc": "{A} <= (NUM)D"},
            0x29: {"op": "KPRI", "A": OP_DST, "B": None, "CD": OP_PRI, "mode": AD_MODE, "desc": "{A} <= D (primitive 0=nil,1=false,2=true)"},
            0x2A: {"op": "KNIL", "A": OP_BASE, "B": None, "CD": OP_BASE, "mode": AD_MODE, "desc": "{A} <= nil, {A+1} <= nil, ..., {D} <= nil"},
            0x2B: {"op": "UGET", "A": OP_DST, "B": None, "CD": OP_UV, "mode": AD_MODE, "desc": "{A} <= uv(D)"},
            0x2C: {"op": "USETV", "A": OP_UV, "B": None, "CD": OP_VAR, "mode": AD_MODE, "desc": "uv(A) <= {D}"},
            0x2D: {"op": "USETS", "A": OP_UV, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "uv(A) <= (STR)D"},
            0x2E: {"op": "USETN", "A": OP_UV, "B": None, "CD": OP_NUM, "mode": AD_MODE, "desc": "uv(A) <= (NUM)D"},
            0x2F: {"op": "USETP", "A": OP_UV, "B": None, "CD": OP_PRI, "mode": AD_MODE, "desc": "uv(A) <= D (primitive 0=nil,1=false,2=true)"},
            0x30: {"op": "UCLO", "A": OP_RBASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Closes uv for slots >= A and JMP => D"},
            0x31: {"op": "FNEW", "A": OP_DST, "B": None, "CD": OP_FUNC, "mode": AD_MODE, "desc": "{A} <= closure(proto(D))"},
            0x32: {"op": "TNEW", "A": OP_DST, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "{A}<=[new_tab[D&0x07FF] new_hash[(D&0xF800)>>11]]"},
            0x33: {"op": "TDUP", "A": OP_DST, "B": None, "CD": OP_TAB, "mode": AD_MODE, "desc": "{A} <= (TAB)D"},
            0x34: {"op": "GGET", "A": OP_DST, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "{A} <= _G[(STR)D] (see getfenv(1))"},
            0x35: {"op": "GSET", "A": OP_VAR, "B": None, "CD": OP_STR, "mode": AD_MODE, "desc": "_G[(STR)D] <= {A} (see getfenv(1))"},
            0x36: {"op": "TGETV", "A": OP_DST, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{A} <= {B}[{C}]"},
            0x37: {"op": "TGETS", "A": OP_DST, "B": OP_VAR, "CD": OP_STR, "mode": BC_MODE, "desc": "{A} <= {B}[(STR)C]"},
            0x38: {"op": "TGETB", "A": OP_DST, "B": OP_VAR, "CD": OP_LIT, "mode": BC_MODE, "desc": "{A} <= {B}[C]"},
            0x39: {"op": "TSETV", "A": OP_VAR, "B": OP_VAR, "CD": OP_VAR, "mode": BC_MODE, "desc": "{B}[{C}] <= {A}"},
            0x3A: {"op": "TSETS", "A": OP_VAR, "B": OP_VAR, "CD": OP_STR, "mode": BC_MODE, "desc": "{B}[(STR)C] <= {A}"},
            0x3B: {"op": "TSETB", "A": OP_VAR, "B": OP_VAR, "CD": OP_LIT, "mode": BC_MODE, "desc": "{B}[C] <= {A}"},
            0x3C: {"op": "TSETM", "A": OP_BASE, "B": None, "CD": OP_MNUM, "mode": AD_MODE, "desc": "{A-1}[(NUM)D],{A-1}[D+1],...<= {A}, {A+1}, ..."},
            0x3D: {"op": "CALLM", "A": OP_BASE, "B": OP_LIT, "CD": OP_LIT, "mode": BC_MODE, "desc": "{A},...,{A+B-2}<={A}({A+1},...,{A+C+MULTRES})"},
            0x3E: {"op": "CALL", "A": OP_BASE, "B": OP_LIT, "CD": OP_LIT, "mode": BC_MODE, "desc": "{A},...,{A+B-2} <= {A}({A+1},...,{A+C-1})"},
            0x3F: {"op": "CALLMT", "A": OP_BASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Tailcall: {A}({A+1},...,{A+D+MULTRES})"},
            0x40: {"op": "CALLT", "A": OP_BASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Tailcall: {A}({A+1},...,{A+D-1})"},
            0x41: {"op": "ITERC", "A": OP_BASE, "B": OP_LIT, "CD": OP_LIT, "mode": BC_MODE, "desc": "Iterator: {A},{A+1},{A+2}<={A-3},{A-2},{A-1};{A},...,{A+B-2} <= {A}({A+1},{A+2})"},
            0x42: {"op": "ITERN", "A": OP_BASE, "B": OP_LIT, "CD": OP_LIT, "mode": BC_MODE, "desc": "Specialized ITERC, if iterator function {A-3} is next()"},
            0x43: {"op": "VARG", "A": OP_BASE, "B": OP_LIT, "CD": OP_LIT, "mode": BC_MODE, "desc": "Vararg: {A},...{A+B-2} <= ..."},
            0x44: {"op": "ISNEXT", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Verify ITERN specialization and jump"},
            0x45: {"op": "RETM", "A": OP_BASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "return {A},...,{A+D+MULTRES-1}"},
            0x46: {"op": "RET", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "return {A},...,{A+D-2}"},
            0x47: {"op": "RET0", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "return"},
            0x48: {"op": "RET1", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "return {A}"},
            0x49: {"op": "FORI", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Numeric for loop init"},
            0x4A: {"op": "JFORI", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Numeric for loop init JIT-compiled"},
            0x4B: {"op": "FORL", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Numeric for loop"},
            0x4C: {"op": "IFORL", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Numeric for loop force interpreter"},
            0x4D: {"op": "JFORL", "A": OP_BASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Numeric for loop JIT-compiled"},
            0x4E: {"op": "ITERL", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Iterator for loop"},
            0x4F: {"op": "IITERL", "A": OP_BASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Iterator for loop force interpreter"},
            0x50: {"op": "JITERL", "A": OP_BASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Iterator for loop JIT-compiled"},
            0x51: {"op": "LOOP", "A": OP_RBASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Generic loop"},
            0x52: {"op": "ILOOP", "A": OP_RBASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Generic loop, force interpreter"},
            0x53: {"op": "JLOOP", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Generic loop,JIT-Compiled"},
            0x54: {"op": "JMP", "A": OP_RBASE, "B": None, "CD": OP_JUMP, "mode": AD_MODE, "desc": "Jump"},
            0x55: {"op": "FUNCF", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Fixed-arg Lua function"},
            0x56: {"op": "IFUNCF", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Fixed-arg Lua function, force interpreter"},
            0x57: {"op": "JFUNCF", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Fixed-arg Lua function JIT-Compiled"},
            0x58: {"op": "FUNCV", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Vararg Lua function"},
            0x59: {"op": "IFUNCV", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Vararg Lua function, force interpreter"},
            0x5A: {"op": "JFUNCV", "A": OP_RBASE, "B": None, "CD": OP_LIT, "mode": AD_MODE, "desc": "Vararg Lua function, JIT-compiled"},
            0x5B: {"op": "FUNCC", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Pseudo-header for C functions"},
            0x5C: {"op": "FUNCCW", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Pseudo-header for wrapped C functions"},
            0x5D: {"op": "FUNC", "A": OP_RBASE, "B": None, "CD": OP_NIL, "mode": AD_MODE, "desc": "Pseudo-header for fast functions"},
        }

    @staticmethod
    def listBC(bytecode, kgc=None, knum=None, upvalues=None):
        outStr = "N\tOP\tA\tB/D\tC\tComment\n"
        i = 1
        for ins in bytecode:
            if len(ins) != 4:
                # An instruction is always a 32-bit word
                continue
            # If the OPCODE >= 5D, we have a fast function pseudo-header
            op = {"op": "FUNC", "A": ByteCode.OP_RBASE, "B": None, "CD": ByteCode.OP_NIL, "mode": ByteCode.AD_MODE, "desc": "Pseudo-header for fast functions"}
            if ins[0] in ByteCode.OPCODE_TABLE.keys():
                op = ByteCode.OPCODE_TABLE[ins[0]]
            outStr += "%03d\t%s\t" % (i,op['op'])
            outStr += "%d\t" % ins[1]
            if op['mode'] == ByteCode.BC_MODE:
                outStr += "%d\t%d\t; %s\n" % (ins[3], ins[2], op["desc"])
            else:
                outStr += "0x%02x%02x\t\t; %s\n" % (ins[3], ins[2], op['desc'])
           
            # FIXME ugly part
            if op["A"] == ByteCode.OP_PRI:
                pri = "true"
                if ins[1] == 0:
                    pri = "nil"
                elif ins[1] == 1:
                    pri = "false"
                outStr += "\t(PRI)A= %s\n" % pri
            if op["B"] == ByteCode.OP_PRI:
                pri = "true"
                if ins[3] == 0:
                    pri = "nil"
                elif ins[3] == 1:
                    pri = "false"
                outStr += "\t(PRI)B= %s\n" % pri
            if op["CD"] == ByteCode.OP_PRI:
                j = 2
                pri = "true"
                if ins[j] == 0:
                    pri = "nil"
                elif ins[j] == 1:
                    pri = "false"
                outStr += "\t(PRI)C/D= %s\n" % pri
            if op["CD"] == ByteCode.OP_JUMP:
                outStr += "\tJMP => %d\n" % (i + 1 + struct.unpack("H", bytes([ins[2],ins[3]]))[0] - 0x8000)
            if kgc is not None:
                # TODO cover other kgc types (tab)
                if op["A"] == ByteCode.OP_STR:
                    outStr += "\t(STR)A= \"%s\"\n" % kgc[ins[1]].getValue()
                if op['B'] == ByteCode.OP_STR:
                    outStr += "\t(STR)B= \"%s\"\n" % kgc[ins[3]].getValue()
                if op['CD'] == ByteCode.OP_STR and op["mode"] == ByteCode.BC_MODE:
                    outStr += "\t(STR)C= \"%s\"\n" % kgc[ins[2]].getValue()
                elif op['CD'] == ByteCode.OP_STR:
                    outStr += "\t(STR)D= \"%s\"\n" % kgc[struct.unpack("<H", ins[2:4])[0]].getValue()
            if knum is not None:
                if op["A"] == ByteCode.OP_NUM:
                    if type(knum[ins[1]]) == int:
                        outStr += "\t(NUM)A= %d\n" % knum[ins[1]]
                    elif type(knum[ins[1]]) == float:
                        outStr += "\t(NUM)A= %f\n" % knum[ins[1]]
                if op['B'] == ByteCode.OP_NUM:
                    if type(knum[ins[3]]) == int:
                        outStr += "\t(NUM)B= %d\n" % knum[ins[3]]
                    elif type(knum[ins[3]]) == float:
                        outStr += "\t(NUM)B= %f\n" % knum[ins[3]]
                if op['CD'] == ByteCode.OP_NUM and op["mode"] == ByteCode.BC_MODE:
                    if type(knum[ins[2]]) == int:
                        outStr += "\t(NUM)C= %d\n" % knum[ins[2]]
                    elif type(knum[ins[2]]) == float:
                        outStr += "\t(NUM)C= %f\n" % knum[ins[2]]
                elif op['CD'] == ByteCode.OP_NUM:
                    val = knum[struct.unpack("<H", ins[2:4])[0]]
                    if type(val) == int:
                        outStr += "\t(NUM)D= %d\n" % val
                    elif type(val) == float:
                        outStr += "\t(NUM)D= %f\n" % val
            i += 1
        return outStr


def read_uleb128(buff):
    # Adapted from https://github.com/LuaJit/LuaJIT/blob/master/src/ls_bcread. l. 136
    print("Reg -> " + str(buff))
    result = buff[0]
    i = 1
    if result >= 0x80:
        shift = 0
        result &= 0x7F
        while True:
            shift += 7
            result |= ((buff[i] & 0x7F) << shift)
            i += 1
            if buff[i-1] < 0x80:
                break
    return [result, i]



def read_uleb128_33(buff):
    # Adapted from https://github.com/LuaJit/LuaJIT/blob/master/src/lj_bcread.c l. 154
    # FIXME not tested yet
    print("33 -> " + str(buff))
    result = buff[0] >> 1
    i = 1
    if result >= 0x40:
        shift = -1
        result &= 0x3F
        while True:
            ch = buff[i]
            shift += 7
            result |= (ch & 0x7F) << shift
            i += 1
            if buff[i-1] < 0x80:
                return [result, i]
    return [result, i]

def hexd(string):
    """
    Helper function to hexdump bytecode
    """
    out_val = ''
    for ch in string:
        out_val += hex(ch) + " "
    return out_val

class Kgc:
    """
    Class to contain KGC values (a type and a value)
    """
    # From: https://github.com/LuaJit/LuaJIT/blob/master/src/lj_obj.h
    # KGC types
    KGC_UNKNOWN = -1
    KGC_CHILD = 0
    KGC_TAB = 1
    KGC_I64 = 2
    KGC_U64 = 3
    KGC_COMPLEX = 4
    KGC_STR = 5

    # ktabk types
    KTAB_NIL = 0
    KTAB_FALSE = 1
    KTAB_TRUE = 2
    KTAB_INT = 3
    KTAB_NUM = 4
    KTAB_STR = 5

    def __init__(self, type_kgc, value):
        self.type_kgc = type_kgc
        self.value = value

    def getType(self):
        return self.type_kgc

    @staticmethod
    def getKtabTypeAsStr(ktype):
        if ktype >= Kgc.KTAB_STR:
            return "KTAB_STR"
        if ktype == Kgc.KTAB_NIL:
            return "KTAB_NIL"
        if ktype == Kgc.KTAB_FALSE:
            return "KTAB_FALSE"
        if ktype == Kgc.KTAB_TRUE:
            return "KTAB_TRUE"
        if ktype == Kgc.KTAB_NUM:
            return "KTAB_NUM"
        if ktype == Kgc.KTAB_INT:
            return "KTAB_INT"
        return "Unknown"

    def getValue(self):
        return self.value

    def toStr(self):
        outStr = "Type: "
        if self.type_kgc == Kgc.KGC_CHILD:
            outStr += "KGC_CHILD\t"
        elif self.type_kgc == Kgc.KGC_TAB:
            outStr += "KGC_TAB\t"
            outStr += "karray length: %d\t khash length: %d\n" % (len(self.value["karray"]), len(self.value["khash"]))
            for el in self.value["karray"]:
                outStr += "\t\ttype: %s\tvalue: " % Kgc.getKtabTypeAsStr(el["type"])
                if el["value"] is None:
                    outStr += "None\n"
                elif el["value"] is False:
                    outStr += "false\n"
                elif el["type"] == Kgc.KTAB_TRUE:
                    outStr += "true\n"
                elif el["type"] == Kgc.KTAB_NUM:
                    outStr += "lo: %d, hi: %d" % (el["value"][0],el["value"][1])
                elif el["type"] >= Kgc.KTAB_STR:
                    outStr += "%s\n" % el["value"].decode('ascii')
                elif el["type"] == Kgc.KTAB_INT:
                    outStr += "%d\n" % el["value"]
                else:
                    outStr += "Should not happen DUH!\n"
            # TODO ugly
            for el in self.value["khash"]:
                outStr += "\t\tkey:" + el["key"].decode('ascii') + "\ttype: %s\tvalue: " % Kgc.getKtabTypeAsStr(el["type"])
                if el["value"] is None:
                    outStr += "None\n"
                elif el["value"] is False:
                    outStr += "false\n"
                elif el["type"] == Kgc.KTAB_TRUE:
                    outStr += "true\n"
                elif el["type"] == Kgc.KTAB_NUM:
                    outStr += "%f" % el['value']
                elif el["type"] >= Kgc.KTAB_STR:
                    outStr += "%s\n" % el["value"].decode('ascii')
                elif el["type"] == Kgc.KTAB_INT:
                    outStr += "%d\n" % el["value"]
                else:
                    outStr += "Should not happen DUH!\n"
        elif self.type_kgc >= Kgc.KGC_STR:
            outStr += "KGC_STR\t"
            outStr += self.value.decode('ascii')
        else: # TODO add the other types here if relevant
            outStr += "%d Not supported yet\n" % self.type_kgc
        return outStr


class GCProto:
    # From: https://github.com/LuaJit/LuaJIT/blob/master/src/lj_obj.h
    # GCProto flags
    PROTO_CHILD = 0x01 # Indicates if there are child prototypes
    PROTO_VARARG = 0x02 # Vararg function
    PROTO_FFI = 0x04 # Uses BC_KCDATA for FFI datatypes
    PROTO_NOJIT = 0x08 # JIT disabled for this function
    PROTO_ILOOP = 0x10 # Patched bytecode with ILOOP, etc...

    def __init__(self, f_strip = True):
        self.flags = 0x00 # Proto's flags
        self.numparams = 0 # Number of parameters
        self.framesize = 0 # Fixed frame size
        self.numuv = 0 # Number of upvalues
        self.numkgc = 0 # Number of collectable constants
        self.numkn = 0 # Number of lua_number constants
        self.numbc = 0 # Number of bytecode instructions
        self.debuglen = 0 # Length of the debugpart in the header
        self.debug_firstline = None # First line of the function definition
        self.debug_numline = None # Number of lines for the function definition
        self.bcins = [] # Bytecode instructions?
        self.uvdata = [] # upvalue list
        self.kgc = [] # Split constant array
        self.knum = [] # Lua number constants
        self.debug = [] # Debug bytes
        self.f_strip = f_strip # If the GCDump Strip flag is set (debug)

    def parseKtabk(self, buff):
        """
        Parses a ktabk entry
        """
        tot_bytes_read = 0
        uleb_buff_size = min([len(buff), MAX_ULEB_SIZE])
        ktabktype, bytes_read = read_uleb128(buff[:uleb_buff_size])
        tot_bytes_read += bytes_read
        return_value = None

        if ktabktype >= Kgc.KTAB_STR:
            str_len = ktabktype - Kgc.KTAB_STR
            ktabk_string = buff[tot_bytes_read:tot_bytes_read+str_len]
            tot_bytes_read += str_len
            return_value = ktabk_string
        elif ktabktype == Kgc.KTAB_INT:
            uleb_buff_size = min([len(buff[tot_bytes_read:]), MAX_ULEB_SIZE])
            return_value, bytes_read = read_uleb128(buff[tot_bytes_read:tot_bytes_read+uleb_buff_size])
            tot_bytes_read += bytes_read
        elif ktabktype == Kgc.KTAB_NUM:
            uleb_buff_size = min([len(buff[tot_bytes_read:]), MAX_ULEB_SIZE])
            lo, bytes_read = read_uleb128(buff[tot_bytes_read:tot_bytes_read+uleb_buff_size])
            tot_bytes_read += bytes_read
            uleb_buff_size = min([len(buff[tot_bytes_read:]), MAX_ULEB_SIZE])
            hi, bytes_read = read_uleb128(buff[tot_bytes_read:tot_bytes_read+uleb_buff_size])
            tot_bytes_read += bytes_read
            return_value = [lo, hi]
        else: # Boolean or nil
            if ktabktype == Kgc.KTAB_TRUE:
                return_value = True
            elif ktabktype == Kgc.KTAB_FALSE:
                return_value = False
            else:
                return_value = None

        return [ktabktype, return_value, tot_bytes_read]

    def parseFromBuff(self, buff):
        """
        Parses buffer to identify proto header and body
        """
        # A proto header has at least 7 bytes, so there is an error if it is less
        if len(buff) < 7:
            return

        # The first header elements are quite simple
        self.flags = buff[0]
        self.numparams = buff[1]
        self.framesize = buff[2]
        self.numuv = buff[3]

        # LEB128 needs some precautions (in case they are multibyte)
        uleb_buff_size = min([len(buff)-4, MAX_ULEB_SIZE])
        self.numkgc, bytes_read = read_uleb128(buff[4:4+uleb_buff_size])
        off = 4 + bytes_read
        uleb_buff_size = min([len(buff)-off, MAX_ULEB_SIZE])
        self.numkn, bytes_read = read_uleb128(buff[off:off+uleb_buff_size])
        off += bytes_read
        uleb_buff_size = min([len(buff)-off, MAX_ULEB_SIZE])
        self.numbc, bytes_read = read_uleb128(buff[off:off+uleb_buff_size])

        # See https://github.com/LuaJIT/LuaJIT/blob/master/src/lj_bcread.c line 357
        # The following code is about debug bytes retrieval
        # I don't interpret them at the moment, this is just to let the pointer increment and skip this part is it exists
        if not self.f_strip:
            off += bytes_read
            uleb_buff_size = min([len(buff)-off, MAX_ULEB_SIZE])
            self.debuglen, bytes_read = read_uleb128(buff[off:uleb_buff_size])
            if self.debuglen > 0:
                off += bytes_read
                uleb_buff_size = min([len(buff)-off,MAX_ULEB_SIZE])
                self.debug_firstline, bytes_read = read_uleb128(buff[off:uleb_buff_size])
                off += bytes_read
                uleb_buff_size = min([len(buff)-off, MAX_ULEB_SIZE])
                self.debug_numline, bytes_read = read_uleb128(buff[off:uleb_buff_size])

        # This is the end of the header
        # The first thing after is the bytecode listing
        # Each instruction has a size of a 32-bit word
        # We don't disassemble the bytecode yet, maybe a bit later
        off += bytes_read
        base = off
        while off < base + self.numbc * 4:
            self.bcins.append(buff[off:off+4])
            off += 4

        # End of the bytecode listing
        # Listing of Upvalue refs (see lua doc about that)
        # Each upvalue is a 16-bit word
        base = off
        while off < base + self.numuv*2:
            self.uvdata.append(struct.unpack("<H", buff[off:off+2])[0]) # TODO verify endianness
            off += 2
        base = off

        # End of the upvalue listing
        # Now we have to handle constants
        # We will create KGCs based on their type here
        # First, we get kgc's type
        # Check https://github.com/LuaJit/LuaJIT/blob/master/src/lj_bcread.c at line 244
        for i in range(0, self.numkgc):
            if len(buff[base:]) == 0:
                self.kgc.append(Kgc(Kgc.KGC_UNKNOWN,""))
                continue
            uleb_buff_size = min([len(buff[base:]), MAX_ULEB_SIZE])
            kgc_type, bytes_read = read_uleb128(buff[base:base+uleb_buff_size])
            base += bytes_read
            kgc_var = None
            # If the type is >= than KGC_STR, then it is a string and its length is the type minus KGC_STR
            if kgc_type >= Kgc.KGC_STR:
                kgc_len = kgc_type - Kgc.KGC_STR
                kgc_string = buff[base:base+kgc_len]
                base += kgc_len
                kgc_var = Kgc(kgc_type, kgc_string)
            elif kgc_type == Kgc.KGC_TAB:
                # Here we read a karray
                uleb_buff_size = min([len(buff[base:]), MAX_ULEB_SIZE])
                narray, bytes_read = read_uleb128(buff[base:base+uleb_buff_size])
                base += bytes_read
                uleb_buff_size = min([len(buff[base:]), MAX_ULEB_SIZE])
                nhash, bytes_read = read_uleb128(buff[base:base+uleb_buff_size])
                base += bytes_read
                ktab = {}
                karray = []
                khash = []
                for i in range(0, narray):
                    val_type, value, bytes_read = self.parseKtabk(buff[base:])
                    karray.append({"type": val_type, "value": value})
                    base += bytes_read
                for i in range(0, nhash):
                    key_type, key, bytes_read = self.parseKtabk(buff[base:])
                    base += bytes_read
                    # No null index. This should not happen on a well-formed BCDump
                    if key_type == Kgc.KTAB_NIL or key_type == Kgc.KTAB_FALSE or key_type == Kgc.KTAB_TRUE:
                        key = "null_key"
                    val_type, value, bytes_read = self.parseKtabk(buff[base:])
                    base += bytes_read
                    khash.append({"type": val_type, "key": key, "value": value})

                ktab = {"karray": karray, "khash": khash}
                kgc_var = Kgc(kgc_type, ktab)
            elif kgc_type != Kgc.KGC_CHILD:
                # TODO As this is only possible if FFI is activated, I didn't implemented it yet
                print("Warn! Something is not implemented here, could crash or yield incorrect results")
                kgc_var = Kgc(kgc_type, "")
            else:
                # Here we assume that kgc_type == Kgc.KGC_CHILD
                # TODO I'm not sure, but it seems this type is dedicated to embed protos as constants
                #print("Warn! Something is not implemented here, could crash or yield incorrect results")
                kgc_var = Kgc(kgc_type, "")
            self.kgc.append(kgc_var)
            # NOTE the 2 last cases will break the lexer if they happen, in case of crash, add the right parsing. My guess is that these are not common

        # Pfiou! End of kgc parsing
        # Let's do knum parsing
        arr = [ hex(x) for x in list(buff) ]
        print("start of num parsing, this is the buffer: " + ' '.join(arr[base:]))
        for i in range(0, self.numkn):
            isnum = buff[base] & 1 
            lo, bytes_read = read_uleb128_33(buff[base:])
            base += bytes_read
            if isnum != 0:    
                hi, bytes_read = read_uleb128(buff[base:])
                base += bytes_read
                self.knum.append(struct.unpack('d', struct.pack('I',lo)+struct.pack('I',hi))[0])
            else:
                self.knum.append(lo)
        self.kgc = list(reversed(self.kgc))
        self.knum = list(reversed(self.knum))

    def toString(self):
        """
        Dumps a proto object to string
        """
        outStr = "---- Proto ----\n"
        outStr += "Flags: "
        if self.flags & GCProto.PROTO_CHILD != 0:
            outStr += "PROTO_CHILD "
        if self.flags & GCProto.PROTO_VARARG != 0:
            outStr += "PROTO_VARARG "
        if self.flags & GCProto.PROTO_FFI != 0:
            outStr += "PROTO_FFI "
        if self.flags & GCProto.PROTO_NOJIT != 0:
            outStr += "PROTO_NOJIT "
        if self.flags & GCProto.PROTO_ILOOP != 0:
            outStr += "PROTO_ILOOP "
        outStr += "\n"
        outStr += "Number of parameters: %d\n" % self.numparams
        outStr += "Frame size: %d\n" % self.framesize
        outStr += "Number of upvalues: %d\n" % self.numuv
        outStr += "Number of collectable constants: %d\n" % self.numkgc
        outStr += "Number of numeric constants: %d\n" % self.numkn
        outStr += "Number of bytecode instructions: %d\n" % self.numbc
        if self.debuglen > 0:
            outStr += "Debug firstline-numline: %d-%d\n" % (self.debug_firstline, self.debug.numline)
        if self.numbc > 0:
            outStr += "Bytecode dump: \n"
        
        outStr += ByteCode.listBC(self.bcins, self.kgc, self.knum)
        #for i in range(0, self.numbc):
        #    outStr += ("%03d\t" % (i+1)) + hexd(self.bcins[i]) + "\n"
       
        if self.numuv > 0:
            outStr += "UVData dump: \n"
        for i in range(0, self.numuv):
            outStr += ("%03d\t" % (i+1)) + "0x %04x" % self.uvdata[i] + "\n"

        outStr += "KGC Vars: \n"
        for i in range(0, len(self.kgc)):
            if self.kgc[i] is not None:
                outStr += "%03d\t%s\n" % (i, self.kgc[i].toStr())

        if self.numkn > 0:
            outStr += "KNum Vars: \n"
        for i in range(0, self.numkn):
            if type(self.knum[i]) is float :
                outStr += "%03d\t%f\n" % (i, self.knum[i])
            elif type(self.knum[i]) is int:
                outStr += "%03d\t%d\n" % (i, self.knum[i])

        return outStr


class BCDump:
    """
    Represents the full bytecode dump (whole file)
    """
    # From: https://github.com/LuaJit/LuaJIT/blob/master/src/lj_bcdump.h
    # Doc: http://wiki.luajit.org/Bytecode-2.0#luajit-2-0-bytecode-dump-format
    
    # Header Flags
    F_BE = 0x01
    F_STRIP = 0x02 # Debug flag (roughly)
    F_FFI = 0x04 # Does the dump depend on FFI?

    def __init__(self):
        # Creates a new empty BCDump object
        self.protos = [] # List of the protos (function blocks)
        self.version = None # Bytecode version (1 in most of the cases)
        self.flags = 0x00 # Dump flags
        self.name = None # Dump name
        self.cleanEnd = False # Did the lexer do the job properly?
        self.f_strip = True # Shortcut to self.flags & F_STRIP

    def getVersion(self):
        """
        Returns the version number
        """
        return self.version

    def getFlags(self):
        """
        Returns the flags
        """
        return self.flags

    def flagsToStr(self):
        """
        Returns the flags as string listing for human reading
        """
        str_flag = ""
        if self.flags & BCDump.F_BE != 0:
            str_flag += 'F_BE '
        if self.flags & BCDump.F_STRIP != 0:
            str_flag += 'F_STRIP '
        if self.flags & BCDump.F_FFI != 0:
            str_flag += 'F_FFI '
        return str_flag

    def getName(self):
        """
        Returns th dump's name
        """
        return self.name

    def getProtos(self):
        """
        Returns the list of protos
        """
        return self.protos

    def isEndClean(self):
        """
        Returns if the lexer ended successfully
        """
        return self.cleanEnd

    def parseFile(self, bin_file):
        """
        Parses an input file as a BC Dump
        """

        # First check the magic bytes and return if wrong format
        first_bytes = bin_file.read(3)
        if first_bytes != b'\x1bLJ':
            print("Not a luaJIT file (wrong magic bytes)!")
            return

        # Get basic information from dump header
        self.version = struct.unpack("B", bin_file.read(1))[0]
        self.flags = struct.unpack("B", bin_file.read(1))[0]
        buff = b''
        nameLength = 0

        # If debug symbols are in file, we have a name
        if self.flags & BCDump.F_STRIP == 0:
            self.f_strip = False
            buff = bin_file.read(MAX_ULEB_SIZE)
            tmp = read_uleb128(buff)
            nameLength = tmp[0]
            i = tmp[1]
            name = ''
            if nameLength >= len(buff[i:]):
                name = buff[i:] + bin_file.read(nameLength-len(buff[i:]))
                buff = b''
                self.name = name.decode('ascii') # Which encoding is really used? This is not specified
            else:
                self.name = buff[i:nameLength+1].decode('ascii') # Same as above
                buff = buff[nameLength+1:]

        # We finished reading the header, let's get all the protos
        still_has_proto = True
        proto_number = 0

        buff = bin_file.read(MAX_ULEB_SIZE-len(buff))

        while still_has_proto:
            if len(buff) < MAX_ULEB_SIZE:
                buff = buff + bin_file.read(MAX_ULEB_SIZE-len(buff))
            # The first part of a proto is its length
            tmp, bytes_read = read_uleb128(buff)
            # If we reach the end of the file before being able to retrieve the announced number of protos
            if len(buff) == 0 or tmp == 0:
                still_has_proto = False
                break
            proto_size = tmp
            bytes_offset = bytes_read
            proto_number += 1 
            # Retrieval of proto bytes
            if proto_size > len(buff[bytes_offset:]):
                proto = buff[bytes_offset:] + bin_file.read(proto_size-len(buff[bytes_offset:]))
                buff = b''
            else:
                proto = buff[bytes_offset:bytes_offset+proto_size]
                buff = buff[proto_size+bytes_offset:]
            # Initialize the proto object
            obj_proto = GCProto(self.f_strip)
            obj_proto.parseFromBuff(proto)
            self.protos.append(obj_proto)
        # If we only have a null byte left, the job is finished
        if buff == b'\x00':
            self.cleanEnd = True


if len(sys.argv) != 2:
    print("Usage: %s <lua_file>" % sys.argv[0])
    sys.exit()

bin_file = open(sys.argv[1], "rb")

bcdump = BCDump()
bcdump.parseFile(bin_file)

if bcdump.getVersion() is None:
    sys.exit()

print("Bytecode version: %d" % bcdump.getVersion())
print("Flags: %s" % bcdump.flagsToStr())
if bcdump.getName() is not None:
    print("File has a name: %s" % bcdump.getName())
i = 1
for proto in bcdump.getProtos():
    print("------------ Proto %d -------------" % i)
    try:
        print(proto.toString())
    except:
        pass
    print()
    i += 1

if bcdump.isEndClean():
    print("Clean end")
