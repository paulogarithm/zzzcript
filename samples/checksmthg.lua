local function foo(n)
    if (n == 1200) then
        print(2)
        return 1
    end
    return 3 * n
end

return foo(3.14)