class PhoneBook
    def initialize
        @names = Array.new(0)
        @numbers = Array.new(0)
        @listed = Array.new(0)
    end

    def add(name, number, is_listed)
        #puts "add"
        if lookupByNum(number)
            if is_listed
                return false
            end
        end
        for i in @names
            if i == name
                return false
            end
        end
        unless  number =~ /\d\d\d-\d\d\d-\d\d\d\d/
            return false
        end
        #conditions
        @names.push(name)
        @numbers.push(number)
        @listed.push(is_listed)
        return true
    end

    def lookup(name)
        #puts "name"
        i = 0
        while i < @names.length
            if @names[i] == name && @listed[i]
                return @numbers[i]
                break
            end
            i += 1
        end
        return nil
    end

    def lookupByNum(number)
        #puts "num"
        i = 0
        while i < @numbers.length
            if @numbers[i] == number && @listed[i]
                return @names[i]
                break
            end
            i += 1
        end
        return nil
    end

    def namesByAc(areacode)
        #puts "ac"
        out = Array.new(0)
        i = 0
        while i < @numbers.length
            #puts "reading #{@numbers[i][0..2]}"
            if @numbers[i][0..2] == areacode
                out.push(@names[i])
            end
            i += 1
        end
        return out
    end
end
