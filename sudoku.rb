require 'minitest/autorun'

#
# A sudoku puzzle is 3x3 grid of 3x3 grids. It is represented here as a single
# array of 3^4 = 81 cells, as that produces a reasonably simple implementation.
#
# To determine the validity of a sudoku grid, it's necessary to extract the
# in a given row, given column, or of the internal 3x3 grids (hereafter referred
# to as a "section"). When needing to address the grid in this way, we construct
# an object that's aware of this relationship and which has a reference back to
# the original complete grid. It can use this reference and a small amount of
# (mostly straightforward) math to refer back to the live cells in the desired
# row, column, or section. This results in a surprisingly clean way to allow
# changes to the original grid to be made indirectly through these methods of
# access.
#
class Sudoku
  DEFAULT_DIMENSION = 3

  attr_reader :dimension
  attr_reader :slots

  #
  # Generate a random, completely-filled Sudoku board. We do this by first
  # creating an empty board, then recursively solving it.
  #
  def self.randomized(dimension = DEFAULT_DIMENSION)
    self.new(dimension).solve!
  end

  #
  # Generate a random Sudoku grid that's been reduced as far as possible (no
  # more values can be removed without allowing multiple solutions).
  #
  def self.puzzle(dimension = DEFAULT_DIMENSION)
    self.randomized(dimension).reduce!
  end

  def initialize(dimension = DEFAULT_DIMENSION)
    self.dimension = dimension
    self.slots     = [nil] * dimension ** 4

    # pre-memoize these before we are frozen
    self.fmt
    self.size
    self.values

    # the interior of slots remains mutable but the slots themselves and
    # our instance data is not
    self.freeze
  end

  def size   = @_size   ||= self.dimension ** 2
  def values = @_values ||= 1.upto(self.size).to_set.freeze

  def dup   = self.class.new(self.dimension).tap { |s| s.slots.replace(self.slots) }
  def clone = self.dup

  def ==(rhs) =
    (self.dimension == rhs.dimension) &&
    (self.slots     == rhs.slots)

  def [] (row, col)      = self.slots[self.index_for(row, col)]
  def []=(row, col, val) = self.slots[self.index_for(row, col)] = val

  def inspect
    # process the slots first to ensure that nils are printed as a blank string
    self.fmt % (self.slots.map { |v| v || '' })
  end

  def row(n)     = Row::new(self, n)
  def col(n)     = Col::new(self, n)
  def section(n) = Section::new(self, n)

  #
  # Iterates over every row and column.
  #
  def each_pair(&block)
    self.size.times.to_a.repeated_permutation(2, &block)
  end

  #
  # We define a Sudoku puzzle to be filled if and only if it contains no empty
  # (nil) slots.
  #
  def filled?
    self.slots.none?(&:nil?)
  end

  #
  # We define a Sudoku puzzle to be legal if and only if it contains no
  # out-of-bounds values in any of its slots. A puzzle containing unset values
  # (nils) is still legal.
  #
  def legal?
    self.slots.to_set.subset?(self.values | [nil])
  end

  #
  # We define a Sudoku puzzle to be congruent if and only if it contains no
  # slots whose value conflicts with that in another slot (e.g., its set of
  # incongruencies is empty).
  #
  def congruent?
    self.incongruencies.empty?
  end

  #
  # We define a Sudoku puzzle to be solved if and only if it is filled, legal,
  # and congruent.
  #
  def solved?
    self.filled? && self.legal? && self.congruent?
  end

  #
  # We define a Sudoku puzzle to be reduced if and only if it clearing any of
  # its slots produces a puzzle which has multiple solutions.
  #
  def reduced?
    self == self.dup.reduce!
  end

  #
  # A Sudoku puzzle is solvable if our solver can solve it. Our solver is
  # perfect™ so this is tautologically true. A puzzle can still be solvable if
  # it has multiple non-unique solutions.
  #
  def solvable?
    # clone the puzzle, try and solve it
    self.dup.solve!.solved?
  end

  #
  # A Sudoku puzzle is unique if it has *exactly one* solution.
  #
  # TODO: this tries an unnecessary third time, get it down to two
  #
  def unique?
    self.solvable? && !self.dup.solve!(2).solved?
  end

  #
  # Returns a `Set` of the legal possibilities for the cell at the given row
  # and column. This method ignores the current contents of the cell. It only
  # performs a basic pass of the core rules of Sudoku; it does not attempt to
  # solve the puzzle in general and it does not consider whether other cells in
  # the same row, column, or section are themselves legal.
  #
  def possibilities(row, col)
    # TODO: since this *actually* writes to `slots`, we have to do
    # bounds-checking on `row` and `col` to not accidentally extend the number
    # of slots we have (for instance, when doing random puzzle generation)
    return Set.new if (
      row >= self.size ||
      col >= self.size
    )

    # we start by assuming all numbers are possible, then we remove any that
    # cause a conflict
    possibilities = self.values.dup
    section       = self.section_of(row, col)

    # Save the current value of the cell. We remove it before checking for
    # conflicts as removing the cell from the slots in its row, column, and
    # section after the fact is "hard".
    current = self[row, col]

    begin
      self[row, col] = nil

      # remove all values that are slots in the same row, column, or section
      possibilities -= self.row(row)        .slots
      possibilities -= self.col(col)        .slots
      possibilities -= self.section(section).slots
    ensure
      # put the cached value back in place
      self[row, col] = current
    end
  end

  #
  # Incongruencies are the list of all slots that contain a value duplicated in
  # their row, column, or section. This function returns `[row, col]` pairs for
  # each such instance.
  #
  # Rows, columns, and sections all allow you to fetch their individual
  # incongruencies by themselves.
  #
  def incongruencies
    self.size.times.reduce(Set.new) do |indices, i|
      indices.merge self.row(i)    .incongruencies
      indices.merge self.col(i)    .incongruencies
      indices.merge self.section(i).incongruencies
    end
  end

  #
  # Solves the puzzle.
  #
  # If `n` is provided and greater than one, returns the nth successful attempt
  # at solving the puzzle instead of the first one discovered. If this method
  # returns a solved puzzle with `n = 2`, the puzzle has muliple non-unique
  # solutions.
  #
  # TODO: Speed this up.
  #
  def solve!(n = 1)
    # quickly abort if the board is incongruent; this will save time in the
    # event that we have a board with an unsolvable partial solution
    return self unless
      self.congruent?

    # collect a list of all unsolved slots
    #
    # NOTE: we tried both grouping these by their section as well as permuting
    # them randomly; both had dramatically worse performance
    unsolved = self.each_pair.select do |row, col|
      self[row, col].nil?
    end

    solver = ->(i) do
      # if we're done but another solution is desired, decrement the counter
      # for the number desired, say we failed, and try again
      return false if
        i >= unsolved.length &&
        (n -= 1) > 0

      # we're done if out-of-bounds for the list of unsolved locations
      return true if
        i >= unsolved.length

      row, col      = unsolved[i]
      possibilities = self.possibilities(row, col)

      # backtrack if there are no legal guesses for the current slot
      return false if
        possibilities.empty?

      # guess randomly until we can recursively generate a legal board; if we
      # fail completely, unset the slot we've been guessing at
      possibilities.to_a.shuffle.detect do |try|
        self[row, col] = try
        solver[i + 1]
      end || self[row, col] = nil
    end

    # run the solver starting at the first unsolved slot
    solver[0]

    # We used to walk through the list of unsolved squares to explicitly delete
    # any guesses we made if we didn't get a solution. This was made unnecessary
    # by the failure condition that unsets each failed slot.
    #
    # Keeping this here for now, in case I'm wrong and we have to bring it back.
    #
    # unsolved.each { |(row, col)| self[row, col] = nil } unless
    #   self.solved?

    self
  end

  #
  # Removes values from slots randomly until there are no more locations where
  # an un-guessed value will still result in a uniquely-solvable puzzle.
  #
  def reduce!
    # abort early if we already contain multiple solutions
    return self if
      self.unique?

    # shuffle all the rows and columns
    locations = self.each_pair.to_a.shuffle

    locations.each do |row, col|
      # cache the value, we may need to set it back if removal doesn't result in
      # in a puzzle with a unique solution
      value = self[row, col]

      # This is a performance improvement. One way to solve this is to remove
      # the element and check if there is one unique solution. Doing this
      # requires *re-solving* for the value we're trying to remove, which is
      # unnecessary work.
      #
      # Instead, we try the remaining possible values for that slot. For each
      # attempt, we see if a solution exists. If one does we know that removing
      # it results in two possible solutions, and so we put the value back.
      # Since every prior step did this check, we also are guaranteed to end up
      # with a puzzle with a single solution.
      unique = (self.possibilities(row, col) - [value]).none? do |v|
        self[row, col] = v
        self.solvable?
      end

      unique ?
        self[row, col] = nil :
        self[row, col] = value
    end

    self
  end

  protected

  attr_writer :dimension
  attr_writer :slots

  def index_for(row, col)
    row * self.size + col
  end

  def section_of(row, col)
    # every third row, the row bumps by one; every third row the column bumps by
    # one
    (row / self.dimension * self.dimension) + (col / self.dimension)
  end

  #
  # The format used when inspecting the grid.
  #
  # We previously used a fixed-size format, but needed to switch to a function
  # in order to support dimensions of Sudoku other than 3. The logic here is
  # long and tedious but ultimately straightforward.
  #
  def fmt
    @_format ||= begin
      dimension = self.dimension
      digits     = Math::log10(self.values.max + 1).ceil
      fmt       = "% #{digits}s"

      #     = the digits           + the separators  + one on either side
      width = (dimension * digits) + (dimension - 1) + 2

      header  = '╔' + (['═' * width] * dimension).join('╤') + '╗' + "\n"
      divider = '╟' + (['─' * width] * dimension).join('┼') + '╢' + "\n"
      footer  = '╚' + (['═' * width] * dimension).join('╧') + '╝' + "\n"

      section = ""   + ([fmt]     * dimension).join(" ")
      row     = "║ " + ([section] * dimension).join(" │ ") + " ║" + "\n"
      rows    = ""   + ([row]     * dimension).join

      "\n" + header + ([rows] * dimension).join(divider) + footer + "\n"
    end
  end
end

class Row
  # TODO: dynamically generate based upon dimension
  FORMAT = [
    '┌───────┬───────┬───────┐',
    '│ %s %s %s │ %s %s %s │ %s %s %s │',
    '└───────┴───────┴───────┘',
  ].join("\n")

  def initialize(sudoku, row)
    self.sudoku = sudoku
    self.row    = row

    self.freeze
  end

  def [] (col)      = self.sudoku[self.row, col]
  def []=(col, val) = self.sudoku[self.row, col] = val

  def slots
    self.sudoku.size.times.map { |i| self[i] }
  end

  def incongruencies
    indices = Hash.new { |h,k| h[k] = [] }

    # we store a map of every value to the indices it's found in; congruent
    # values will only have *one* such index
    self.slots.each.with_index { |v, i| indices[v] << [row, i] }

    indices
      .reject { |k, v| k.nil? }        # not interested in nil values
      .reject { |k, v| v.length == 1 } # not interested in unique values
      .map    { |k, v| v }             # we only need the indices themselves
      .reduce([], &:+)                 # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    FORMAT % self.slots.map { |v| v.nil? ? ' ' : v.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :row
end

class Col
  # TODO: dynamically generate based upon dimension
  FORMAT =  [
    '┌───┐',
    '│ %s │',
    '│ %s │',
    '│ %s │',
    '├───┤',
    '│ %s │',
    '│ %s │',
    '│ %s │',
    '├───┤',
    '│ %s │',
    '│ %s │',
    '│ %s │',
    '└───┘',
  ].join("\n")

  def initialize(sudoku, col)
    self.sudoku = sudoku
    self.col    = col

    self.freeze
  end

  def [] (row)      = self.sudoku[row, self.col]
  def []=(row, val) = self.sudoku[row, self.col] = val

  def slots
    self.sudoku.size.times.map { |i| self[i] }
  end

  def incongruencies
    indices = Hash.new { |h,k| h[k] = [] }

    self.slots.each.with_index { |v, i| indices[v] << [i, col] }

    indices
      .reject { |k, v| k.nil? }
      .reject { |k, v| v.length == 1 }
      .map    { |k, v| v }
      .reduce([], &:+) # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    FORMAT % self.slots.map { |v| v.nil? ? ' ' : v.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :col
end

class Section
  # TODO: dynamically generate based upon dimension
  FORMAT = [
    '┌───────┐',
    '│ %s %s %s │',
    '│ %s %s %s │',
    '│ %s %s %s │',
    '└───────┘',
  ].join("\n")

  def initialize(sudoku, index)
    dimension = sudoku.dimension

    self.sudoku = sudoku
    self.row    = (index / dimension) * dimension
    self.col    = (index % dimension) * dimension

    self.freeze
  end

  def [] (row, col)      = self.sudoku[self.row + row, self.col + col]
  def []=(row, col, val) = self.sudoku[self.row + row, self.col + col] = val

  def each_pair(&block)
    self.sudoku.dimension.times.to_a.repeated_permutation(2, &block)
  end

  def slots
    self.each_pair.map do |row, col|
      self[row, col]
    end.flatten
  end

  def incongruencies
    indices = Hash.new { |h,k| h[k] = [] }

    self.slots.each_slice(3).with_index do |row, i|
      row.each.with_index do |v, j|
        indices[v] << [self.row + i, self.col + j]
      end
    end

    indices
      .reject { |k, v| k.nil? }
      .reject { |k, v| v.length == 1 }
      .map    { |k, v| v }
      .reduce([], &:+) # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    FORMAT % self.slots.map { |v| v.nil? ? ' ' : v.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :row
  attr_accessor :col
end

class TestSudoku
  class TestEmpty < Minitest::Test
    def sudoku
      @sudoku ||= Sudoku.new
    end

    def test_empty
      sudoku.each_pair.map do |row, col|
        assert_nil sudoku[row, col]
      end
    end

    def test_indirectly_empty
      sudoku.each_pair.map do |i, j|
        assert_nil sudoku.row(i)[j]
        assert_nil sudoku.col(i)[j]
        assert_nil sudoku.section(i)[j / sudoku.dimension, j % sudoku.dimension]
      end
    end

    def test_unsolved
      refute_predicate sudoku, :solved?
    end

    def test_no_incongruencies
      assert_empty sudoku.incongruencies
    end

    def test_unlimited_possibilities
      sudoku.each_pair.map do |row, col|
        assert_equal sudoku.values, sudoku.possibilities(row, col)
      end
    end

    def test_indexing
      sudoku[4, 7] = 9

      assert_equal 9, sudoku[4, 7]
    end

    def test_indirect_indexing
      sudoku[6, 2] = 4

      assert_equal 4, sudoku.row(6)[2]
      assert_equal 4, sudoku.col(2)[6]
      assert_equal 4, sudoku.section(6)[0, 2]
    end

    def test_indirect_assignment
      sudoku.row(1)[5]        = 1
      sudoku.col(8)[7]        = 2
      sudoku.section(2)[2, 0] = 3

      assert_equal 1, sudoku[1, 5]
      assert_equal 2, sudoku[7, 8]
      assert_equal 3, sudoku[2, 6]
    end
  end

  class TestSolved < Minitest::Test
    def sudoku
      # TODO: a nice way to construct pre-made grids would be nice, but I don't
      # feel excited by any I've come up with yet so this will do for now
      @sudoku ||= Sudoku.new.tap do |s|
        s.slots.replace [
          7, 4, 9, 5, 8, 1, 3, 2, 6,
          1, 8, 2, 7, 3, 6, 9, 5, 4,
          6, 3, 5, 9, 4, 2, 1, 7, 8,
          5, 7, 6, 3, 9, 4, 8, 1, 2,
          4, 2, 8, 1, 6, 7, 5, 3, 9,
          3, 9, 1, 2, 5, 8, 6, 4, 7,
          8, 6, 3, 4, 7, 5, 2, 9, 1,
          2, 5, 4, 8, 1, 9, 7, 6, 3,
          9, 1, 7, 6, 2, 3, 4, 8, 5
        ]
      end
    end

    def test_solved
      assert_predicate sudoku, :solved?
    end

    def test_no_incongruencies
      assert_empty sudoku.incongruencies
    end

    def test_one_possibility
      sudoku.each_pair.map do |row, col|
        assert_equal Set[sudoku[row, col]], sudoku.possibilities(row, col)
      end
    end

    def test_un_solved
      sudoku[2, 3] = nil

      refute_predicate sudoku, :solved?
      assert_empty     sudoku.incongruencies
    end

    def test_injected_incongruency
      sudoku[4, 4] = 2

      refute_predicate sudoku, :solved?
      refute_empty     sudoku.incongruencies

      expected = Set[
        [4, 4],
        [4, 1],
        [8, 4],
        [5, 3],
      ]

      assert_equal expected, sudoku.incongruencies
    end

    def test_re_solved
      value        = sudoku[8, 4]
      sudoku[8, 4] = value.succ
      refute_predicate sudoku, :solved?

      sudoku[8, 4] = value
      assert_predicate sudoku, :solved?
    end

    def test_value_out_of_bounds
      sudoku[1, 4] = 0

      refute_predicate sudoku, :solved?
      assert_empty     sudoku.incongruencies
    end
  end

  class TestUnsolvable < Minitest::Test
    def sudoku
      @sudoku ||= Sudoku.new.tap do |s|
        s.slots.replace [
          5,   1,   6,   8,   4,   9,   7,   3,   2,
          3, nil,   7,   6, nil,   5, nil, nil, nil,
          8, nil,   9,   7, nil, nil, nil,   6,   5,
          1,   3,   5, nil,   6, nil,   9, nil,   7,
          4,   7,   2,   5,   9,   1, nil, nil,   6,
          9,   6,   8,   3,   7, nil, nil,   5, nil,
          2,   5,   3,   1,   8,   6, nil,   7,   4,
          6,   8,   4,   2, nil,   7,   5, nil, nil,
          7,   9,   1, nil,   5, nil,   6, nil,   8,
        ]
      end
    end

    def test_unsolvable
      refute_predicate sudoku, :solvable?
    end

    def test_remains_unsolved
      refute_predicate sudoku.solve!, :solved?
    end
  end

  class TestDimensionOne < Minitest::Test
    def sudoku
      @sudoku ||= Sudoku.new(1)
    end

    def test_only_one_solution
      assert_equal Set[1], sudoku.possibilities(0, 0)
    end

    def test_solved
      sudoku[0, 0] = 1

      assert_predicate sudoku, :solved?
    end

    def test_out_of_bounds
      sudoku[0, 0] = 2

      refute_predicate sudoku, :solved?
    end
  end

  class TestSolvable < Minitest::Test
    def sudoku
      @sudoku ||= Sudoku.new.tap do |s|
        s.slots.replace [
          nil, nil, nil,   2, nil, nil, nil,   8,   6,
            1, nil, nil,   8,   9, nil, nil, nil,   7,
          nil,   3, nil, nil, nil, nil,   9,   1,   2,
            3,   4,   5,   1,   7, nil,   6, nil, nil,
          nil,   9, nil, nil,   5,   2, nil, nil, nil,
            7, nil,   6,   9,   4,   3, nil,   5,   8,
          nil, nil, nil, nil, nil,   4, nil, nil,   5,
          nil, nil,   7,   5,   1,   9, nil,   4,   3,
          nil, nil,   4,   3, nil,   6, nil, nil, nil,
        ]
      end
    end

    def test_solvable
      assert_predicate sudoku, :solvable?
    end
  end

  class TestRandomized < Minitest::Test
    def sudoku
      @sudoku ||= Sudoku.randomized
    end

    def test_congruent
      assert_predicate sudoku, :congruent?
    end

    def test_filled
      assert_predicate sudoku, :filled?
    end

    def test_legal
      assert_predicate sudoku, :legal?
    end

    def test_solved
      assert_predicate sudoku, :solved?
    end
  end

  class TestPuzzle < Minitest::Test
    def sudoku
      # TODO: bump to higher puzzle sizes when it's fast enough
      @sudoku ||= Sudoku.puzzle(2)
    end

    def test_puzzle_solvable
      assert_predicate sudoku, :solvable?
    end

    def test_puzzle_unfilled
      refute_predicate sudoku, :filled?
    end

    def test_puzzle_reduced
      assert_predicate sudoku, :reduced?
    end
  end
end
