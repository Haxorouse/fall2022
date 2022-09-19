require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    board = GameBoard.new(10, 10)
    worked = read_file_lines(path){ |line|
        vals = line.split(",")
        row = vals[0].tr("(", "")
        row = row.to_i
        col = vals[1].tr(")", "")
        col = col.to_i
        dir = vals[2].tr(" ", "")
        len = vals[3].tr(" ", "")
        len = len.to_i
        pos = Position.new(row, col)
        ship = Ship.new(pos, dir, len)
        return nil unless board.add_ship(ship)
    }
    return nil unless worked
    return board
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    #puts("reading attack start")
    attacks = []
    worked = read_file_lines(path){ |line|
        #puts("reading attack")
        if line.match(/\A\(\d+,\d+\)/)
            #puts "past if"
            vals = line.split(",")
            row = vals[0].tr("(", "")
            row = row.to_i
            col = vals[1].tr(")", "")
            col = col.to_i
            attacks.push(Position.new(row, col))
        end
    }
    #puts attacks
    return nil unless worked
    return attacks
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
