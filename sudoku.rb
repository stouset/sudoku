# frozen_string_literal: true

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
    self.slots     = [nil] * (dimension ** 4)

    # pre-memoize before freeze
    self.fmt
    self.size
    self.values

    # the interior of slots remains mutable but the slots themselves and
    # our instance data is not
    self.freeze
  end

  def [] (row, col)      = self.slots[self.index_for(row, col)]
  def []=(row, col, val) = self.slots[self.index_for(row, col)] = val

  #
  # Two Sudoku grids are equal if they have the same dimension and
  # contents.
  #
  def ==(other)
    (self.dimension == other.dimension) && (self.slots == other.slots)
  end

  #
  # The width, height, and number of sections in this Sudoku grid.
  #
  def size
    @_size ||= self.dimension ** 2
  end

  #
  # The set of legal values in this Sudoku grid.
  #
  def values
    @_values ||= 1.upto(self.size).to_set.freeze
  end

  #
  # Duplicates a Sudoku grid, copying its dimensions and slots into the
  # new one.
  #
  def dup
    dup = self.class.new(self.dimension)
    dup.slots.replace(self.slots)
    dup
  end

  alias_method :clone, :dup

  def inspect
    # process the slots first to ensure that nils are printed as a blank string
    self.fmt % (self.slots.map { it || '' })
  end

  def row(row)      = Row::new(self, row)
  def col(col)      = Col::new(self, col)
  def section(sect) = Section::new(self, sect)

  #
  # Iterates over every row and column pair.
  #
  def each_index(&)
    self.size.times.to_a.repeated_permutation(2, &)
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
    self == self.reduce
  end

  #
  # A Sudoku puzzle is solvable if our solver can solve it. Our solver is
  # perfect™ so this is tautologically true. A puzzle can still be solvable if
  # it has multiple non-unique solutions.
  #
  def solvable?
    # clone the puzzle, try and solve it
    self.solve.solved?
  end

  #
  # A Sudoku puzzle is unique if it has *exactly one* solution.
  #
  def unique?
    # TODO: this tries an unnecessary third time; we should rewrite `solve` so
    # we can (in one pass) distinguish between a puzzle that was unsolvable,
    # solved once, or solved twice
    self.solvable? && !self.solve(2).solved?
  end

  #
  # Returns a `Set` of the legal possibilities for the cell at the given row
  # and column. This method ignores the current contents of the cell. It only
  # performs a basic pass of the core rules of Sudoku; it does not attempt to
  # solve the puzzle in general and it does not consider whether other cells in
  # the same row, column, or section are themselves legal.
  #
  def possibilities(row, col)
    # calculate the section of the row and column, and also the index of that
    # "global" row and column within that specific section's slots
    #
    # TODO: section_index seems like it ought to be a helper method on Section
    section        = self.section_of(row, col)
    section_index  = (row % self.dimension * self.dimension) + (col % self.dimension)

    # given the set of all possible values, remove any values that occur in the
    # same row, column, or section (*except* for ones at the location we're
    # trying to investigate)
    self.values.dup.tap do |possibilities|
      possibilities.subtract(self.row(row)        .slots.tap { it[col] = nil })
      possibilities.subtract(self.col(col)        .slots.tap { it[row] = nil })
      possibilities.subtract(self.section(section).slots.tap { it[section_index] = nil })
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
  # Returns the current puzzle unmodified if it is already solved or if
  # it is unsolvable.
  #
  # If `n` is provided and greater than one, returns the nth successful attempt
  # at solving the puzzle instead of the first one discovered. If this method
  # returns a solved puzzle with `n = 2`, the puzzle has muliple non-unique
  # solutions.
  #
  # TODO: Speed this up. Also `n = 0` is meaningless and solves the puzzle
  # anyway. Reconsider the semantics of this parameter.
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
    unsolved = self.each_index.select do |row, col|
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

    self
  end

  #
  # Returns a copy of the current Sudoku grid but solved.
  #
  def solve(*)
    self.dup.solve!(*)
  end

  #
  # Removes values from slots randomly until there are no more locations where
  # an un-guessed value will still result in a uniquely-solvable puzzle.
  #
  # Returns the current puzzle unmodified if it is already reduced or if it
  # already allows for multiple solutions.
  #
  def reduce!
    # abort early if we already contain multiple solutions
    return self if
      self.unique?

    # shuffle all the rows and columns to produce a non-deterministic
    # reduced result
    indices = self.each_index.to_a.shuffle

    indices.each do |row, col|
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

      # if the current value is a unique solution, we can remove it; otherwise
      # we have to set it back to what it was originally
      self[row, col] = unique ? nil : value
    end

    self
  end

  #
  # Returns a copy of the current Sudoku grid but solved.
  #
  def reduce
    self.dup.reduce!
  end

  protected

  attr_writer :dimension
  attr_writer :slots

  def index_for(row, col)
    (row * self.size) + col
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
    @_fmt ||= begin
      dimension = self.dimension
      digits    = Math::log10(self.values.max + 1).ceil
      fmt       = "% #{digits}s"

      #     = the digits           + the separators  + one on either side
      width = (dimension * digits) + (dimension - 1) + 2

      header  = '╔' + (['═' * width] * dimension).join('╤') + '╗' + "\n"
      divider = '╟' + (['─' * width] * dimension).join('┼') + '╢' + "\n"
      footer  = '╚' + (['═' * width] * dimension).join('╧') + '╝' + "\n"

      section = ''   + ([fmt]     * dimension).join(' ')
      row     = '║ ' + ([section] * dimension).join(' │ ') + ' ║' + "\n"
      rows    = ''   + ([row]     * dimension).join

      "\n" + header + ([rows] * dimension).join(divider) + footer + "\n"
    end
  end
end

class Row
  def initialize(sudoku, row)
    self.sudoku = sudoku
    self.row    = row

    # pre-memoize before freeze
    self.fmt

    self.freeze
  end

  def [] (col)      = self.sudoku[self.row, col]
  def []=(col, val) = self.sudoku[self.row, col] = val

  def slots
    self.sudoku.size.times.map { self[it] }
  end

  def incongruencies
    dupes = Hash.new { |h, k| h[k] = [] }

    # we store a map of every value to the indices it's found in; congruent
    # values will only have *one* such index
    self.slots.each.with_index { |value, col| dupes[value] << [self.row, col] }

    dupes
      .reject { |value, _indices| value.nil? }
      .reject { |_value, indices| indices.length == 1 }
      .values
      .reduce([], &:+) # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    self.fmt % self.slots.map { it.nil? ? ' ' : it.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :row

  def fmt
    @_fmt ||= begin
      dimension = self.sudoku.dimension
      digits    = Math::log10(self.sudoku.values.max + 1).ceil
      fmt       = "% #{digits}s"

      #     = the digits           + the separators  + one on either side
      width = (dimension * digits) + (dimension - 1) + 2

      header  = '┌' + (['─' * width] * dimension).join('┬') + '┐' + "\n"
      footer  = '└' + (['─' * width] * dimension).join('┴') + '┘' + "\n"

      section = ''   + ([fmt]     * dimension).join(' ')
      row     = '│ ' + ([section] * dimension).join(' │ ') + ' │' + "\n"

      "\n" + header + row + footer + "\n"
    end
  end
end

class Col
  def initialize(sudoku, col)
    self.sudoku = sudoku
    self.col    = col

    # pre-memoize before freeze
    self.fmt

    self.freeze
  end

  def [] (row)      = self.sudoku[row, self.col]
  def []=(row, val) = self.sudoku[row, self.col] = val

  def slots
    self.sudoku.size.times.map { self[it] }
  end

  def incongruencies
    dupes = Hash.new { |h, k| h[k] = [] }

    # we store a map of every value to the indices it's found in; congruent
    # values will only have *one* such index
    self.slots.each.with_index { |value, row| dupes[value] << [row, self.col] }

    dupes
      .reject { |value, _indices| value.nil? }
      .reject { |_value, indices| indices.length == 1 }
      .values
      .reduce([], &:+) # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    self.fmt % self.slots.map { it.nil? ? '' : it.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :col

  def fmt
    @_fmt ||= begin
      dimension = self.sudoku.dimension
      digits    = Math::log10(self.sudoku.values.max + 1).ceil
      fmt       = "% #{digits}s"

      #     = the digits + one on either side
      width = digits     + 2

      header  = '┌' + ['─' * width].join + '┐' + "\n"
      divider = '├' + ['─' * width].join + '┤' + "\n"
      footer  = '└' + ['─' * width].join + '┘' + "\n"

      row     = '│ ' + fmt + ' │' + "\n"
      rows    = ''   + ([row] * dimension).join

      "\n" + header + ([rows] * dimension).join(divider) + footer + "\n"
    end
  end
end

class Section
  def initialize(sudoku, index)
    dimension = sudoku.dimension

    self.sudoku = sudoku
    self.row    = (index / dimension) * dimension
    self.col    = (index % dimension) * dimension

    # pre-memoize before freeze
    self.fmt

    self.freeze
  end

  def [] (row, col)      = self.sudoku[self.row + row, self.col + col]
  def []=(row, col, val) = self.sudoku[self.row + row, self.col + col] = val

  def each_index(&)
    self.sudoku.dimension.times.to_a.repeated_permutation(2, &)
  end

  def slots
    self.each_index.map do |row, col|
      self[row, col]
    end.flatten
  end

  def incongruencies
    dupes = Hash.new { |h, k| h[k] = [] }

    # we store a map of every value to the indices it's found in; congruent
    # values will only have *one* such index
    self.slots.each_slice(3).with_index do |slice, row|
      slice.each.with_index do |value, col|
        dupes[value] << [self.row + row, self.col + col]
      end
    end

    dupes
      .reject { |value, _indices| value.nil? }
      .reject { |_value, indices| indices.length == 1 }
      .values
      .reduce([], &:+) # quick and dirty single-level flatten
      .to_set
  end

  def inspect
    self.fmt % self.slots.map { it.nil? ? '' : it.to_s }
  end

  protected

  attr_accessor :sudoku
  attr_accessor :row
  attr_accessor :col

  def fmt
    @_fmt ||= begin
      dimension = self.sudoku.dimension
      digits    = Math::log10(self.sudoku.values.max + 1).ceil
      fmt       = "% #{digits}s"

      #     = the digits           + the separators  + one on either side
      width = (dimension * digits) + (dimension - 1) + 2

      header  = '┌' + ['─' * width].join + '┐' + "\n"
      footer  = '└' + ['─' * width].join + '┘' + "\n"

      section = ''   + ([fmt]     * dimension).join(' ')
      row     = '│ ' + section + ' │' + "\n"
      rows    = ''   + ([row]     * dimension).join

      "\n" + header + rows + footer + "\n"
    end
  end
end

class TestSudoku
  class TestEmpty < Minitest::Test
    def sudoku
      @_sudoku ||= Sudoku.new
    end

    def test_empty
      sudoku.each_index.map do |row, col|
        assert_nil sudoku[row, col]
      end
    end

    def test_empty_rows
      sudoku.each_index.map do |row, col|
        assert_nil sudoku.row(row)[col]
      end
    end

    def test_empty_columns
      sudoku.each_index.map do |row, col|
        assert_nil sudoku.col(col)[row]
      end
    end

    def test_empty_sections
      sudoku.each_index.map do |i, j|
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
      sudoku.each_index.map do |row, col|
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
      @_sudoku ||= Sudoku.new.tap do |s|
        s.slots.replace [
          7, 4, 9, 5, 8, 1, 3, 2, 6,
          1, 8, 2, 7, 3, 6, 9, 5, 4,
          6, 3, 5, 9, 4, 2, 1, 7, 8,
          5, 7, 6, 3, 9, 4, 8, 1, 2,
          4, 2, 8, 1, 6, 7, 5, 3, 9,
          3, 9, 1, 2, 5, 8, 6, 4, 7,
          8, 6, 3, 4, 7, 5, 2, 9, 1,
          2, 5, 4, 8, 1, 9, 7, 6, 3,
          9, 1, 7, 6, 2, 3, 4, 8, 5,
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
      sudoku.each_index.map do |row, col|
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
      @_sudoku ||= Sudoku.new.tap do |s|
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
      @_sudoku ||= Sudoku.new(1)
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
      @_sudoku ||= Sudoku.new.tap do |s|
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
      @_sudoku ||= Sudoku.randomized
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
      @_sudoku ||= Sudoku.puzzle(2)
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
