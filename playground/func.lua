_G.__metahello = {
    __call = function(_, n)
        if (type(n) ~= "number") then
            error("invalid type")
        end
        print("hello")
    end
}
_G.hello = _G.setmetatable({
    [3] = function()
        print("waza")
    end,
    [10] = function()
        print("coco")
    end
}, _G.__metahello)

local n = 1

if _G.hello[n] then _G.hello[n]() else _G.hello(n) end