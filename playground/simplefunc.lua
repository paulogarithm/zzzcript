local function hello(n)
    if (type(n) ~= "number") then
        error()
    end
    if (n == 3) then
        print("waza")
        return
    end
    if (n == 10) then
        print("coco")
        return
    end
    print(n)
end

local n = 1
hello(n)