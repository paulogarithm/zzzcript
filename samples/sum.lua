local function sum(xs)
    if (#xs == 0) then
        return 0;
    end
    local x = table.remove(xs, 1)
    return x + sum(xs)
end

local xs = {10, 3, 7, 12}
print(sum(xs))