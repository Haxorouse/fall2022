class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column
    @ships
    @board #2D array of bools, true if has ship
    @attacks#stores position of all attacks(key) and if it was a hit(value)
    @hits
    @shipTiles

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @board = Array.new
        @ships = Array.new
        @attacks = {}
        @hits = 0
        @shipTiles = 0
        i = 0
        j = 0
        while i < max_row do
            newCol = []
            @board.push(newCol)
            while j < max_column do
                @board[i].push(false)
                j += 1
            end
            i += 1
        end
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        shipPoss = [ship.start_position]
        r = ship.start_position.row
        c = ship.start_position.column
        i = 1
        if ship.orientation == "Up"
            while i < ship.size do
                shipPoss.push(Position.new(r - i, c))
                i += 1
            end
        end
        if ship.orientation == "Down"
            while i < ship.size do
                shipPoss.push(Position.new(r + i, c))
                i += 1
            end
        end
        if ship.orientation == "Left"
            while i < ship.size do
                shipPoss.push(Position.new(r, c - i))
                i += 1
            end
        end
        if ship.orientation == "Right"
            while i < ship.size do
                shipPoss.push(Position.new(r, c + i))
                i += 1
            end
        end
        for p in shipPoss do
            if p.row < 1 || p.row > @max_row || p.column < 1 || p.column > @max_column
                return false
            end
            if @board[p.row - 1][p.column - 1] == true 
                return false
            end
        end
        @ships.push(ship)
        for p in shipPoss do
            @board[p.row - 1][p.column - 1] = true
            @shipTiles += 1
        end
        return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        r = position.row
        c = position.column
        if r < 1 || r > @max_row || c < 0 || c > @max_column
            return nil
        end
        if @board[r - 1][c - 1] == true
            @attacks[:position] = true
            @hits += 1
            return true
        end
        @attacks[:position] = false
        return false
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        @hits
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        if @hits == @shipTiles then return true
            return false
        end
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        "STRING METHOD IS NOT IMPLEMENTED"
    end
end
