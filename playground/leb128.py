import struct

# bytes_float = b'\x40\x00\x00\x00'
# str_float = bytes_float.decode('hex')  # or use a more robust parsing method
# float_value = struct.unpack('f', bytes.fromhex(str_float))[0]
# print(float_value)

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

if __name__ == "__main__":
    # array = b'\xbf\x94\xdc\x9e\x0a\xb8\xbd\xa4\x80\x04'
    # array = b'\xb8\xbd\xa4\x80\x04'
    # array = [0xb8, 0xbd, 0xa4, 0x80, 0x04, 0x00]
    knum = []
    # array = [0xbf, 0x94, 0xdc, 0x9e, 0xa, 0xb8, 0xbd, 0xa4, 0x80, 0x4]
    # array = [5,143,174,138,159,4,128,164,189,184]
    array = [159,138,174,143,5,184,189,164,128,4]
    base = 0
    isnum = array[base] & 1 
    lo, bytes_read = read_uleb128_33(array[base:])
    base += bytes_read
    if isnum != 0:    
        hi, bytes_read = read_uleb128(array[base:])
        base += bytes_read
        knum.append(struct.unpack('d', struct.pack('I', lo) + struct.pack('I', hi))[0])
    else:
        knum.append(lo)
    print(knum)
    # num, foo = read_uleb128(array)
    # print([int(b) for b in num.to_bytes(4, byteorder='little')])
    # print(num, foo)
    # print(struct.unpack('<f', num.to_bytes(4, byteorder='little')))
