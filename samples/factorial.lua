local function fact(n)
    if (n == 0) then
        return 1
    end
    if (n == 1) then
        return 1
    end
    return n * fact(n - 1)
end

print(fact(5))