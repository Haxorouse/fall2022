def fib(n)
    #puts "fib"
    out = Array.new(0)
    if n > 0
        out.push(0)
    end
    if n > 1
        out.push(1)
    end
    if n > 2
        i = 2
        while i < n do
            out.push(out[-1] + out[-2])
            i += 1
        end
    end
    return out
end

def isPalindrome(n)
    #puts "pal"
    n = n.to_s.split(//)
    while n.length > 1
        a = n.pop
        b = n.shift
        if a != b
            return false
        end
    end
    return true
end

def nthmax(n, a)
    #puts "nmax"
    a.sort!
    a.reverse!
    return a[n]
end

def freq(s)
    #puts "freq"
    a = s.split(//)
    if a.length == 0
        return ""
    end
    a.sort!
    current = 1
    max = 1
    check = a.shift
    maxChar = check
    while a.length > 0 do
        test = a.shift
        if test != check
            if current > max
                max = current
                maxChar = check
            end
            check = test
            current = 1
        else
            current += 1
        end
    end
    if current > max
        max = current
        maxChar = check
    end
    return maxChar
end

def zipHash(arr1, arr2)
    #puts "zipHash"
if arr1.length != arr2.length
    return nil
end
out = Hash[]
while arr1.length > 0 do
    out[arr1.shift] = arr2.shift
end
return out
end

def hashToArray(hash)
    #puts "hash to array"
    keys = hash.keys
    out = Array.new(0)
    while keys.length > 0 do
        key = keys.shift
        element = Array[key, hash.fetch(key)]
        out.push(element)
    end
    return out
end
