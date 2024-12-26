-- a = 3.14
-- a = 3

local function rec(n)
    return rec(n - 1)
end

--[[

    1b 4c 4a            // header
    02 02 24 02         // ???
    00 01 00 02         // ???
    01 06               // ???

    RR -> Register
    LL -> Literal
    GG -> Global
    KK -> Constant
    
    ISGE    0x01        // 01 RR RR __
    ISGT    0x03        // 01 RR RR __
    JMP     0x58        // 58 ?? LL ??      // ???, instructions, ???
    KSHORT  0x29        // 29 RR LL=LL
    GGET    0x36        // 36 RR GG 00
    GSET    0x37        // 37 RR GG 00
    ADDNV   0x1b        // 1b RR KK RR
    ADDVN   0x16        // 1b RR RR KK
    CALL    0x42        // 42 RR LL LL      // dst, nret+1, nargs+1
    RET0    0x4b        // 4b LL LL __

    // constants

    How to read a float ?
    decimal             => 3.14
    luajit              => bf 94 dc 9e 0a b8 bd a4 80 04
    luajit / 2          => 5f 4a 6e 47 05 5c 5e 52 40 02
    C double            => 40 09 1e b8 51 eb 85 1f
    C double lilendian  => 1f 85 eb 51 b8 1e 09 40


--]]