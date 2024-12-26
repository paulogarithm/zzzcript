import struct

NUMS = [0xbf, 0x94, 0xdc, 0x9e, 0xa, 0xb8, 0xbd, 0xa4, 0x80, 0x4]

def read_uleb128(buff):
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

def main():
    knums = 0
    buff = NUMS
    base = 0
    isnum = buff[base] & 1 
    lo, bytes_read = read_uleb128_33(buff[base:])
    base += bytes_read
    if isnum != 0:    
        hi, bytes_read = read_uleb128(buff[base:])
        base += bytes_read
        knums = struct.unpack('d', struct.pack('I', lo) + struct.pack('I', hi))[0]
    else:
        knums = (lo)
    print(knums)

if __name__ == "__main__":
    main()
