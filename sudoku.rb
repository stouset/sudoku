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
# TODO: We could actually pretty easily make the Sudoku "dimension" size dynamic
# for a dimension-2 4x4 grid, dimension-3 9x9 grid, dimension-4 16x16 grid, etc.
# I started down this path and even had the `FORMAT` string written to do this.
# But then I realized that a dimension-4 grid would require two-character values
# and so it would need to adapt to the growing width of the values themselves. I
# could have used hex for dimension-4 grids and Base64 for all the way up to
# dimension-8 grids! But then what if someone wanted a dimension-9 (81x81 grid)?
# Won't somebody please think of the Computer Scientists?
#
# Also vaguely interesting: a dimension-1 Sudoku grid has one cell and it must
# be 1. Challenging!
class Sudoku
  DIMENSION = 3
  SIZE      = DIMENSION ** 2
  SLOTS     = DIMENSION ** 4
  VALUES    = 1.upto(SIZE).to_set.freeze

  #
  # The format used when inspecting the grid. It looks misaligned, but it isn't
  # (the %s format string is two characters long while the corresponding grid
  # borders themselves are only one).
  #
  FORMAT = [
    '╔═══════╤═══════╤═══════╗',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '╟───────┼───────┼───────╢',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '╟───────┼───────┼───────╢',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '║ %s %s %s │ %s %s %s │ %s %s %s ║',
    '╚═══════╧═══════╧═══════╝'
  ].join("\n")

  def initialize
    self.slots = [nil] * SLOTS
    self.freeze # the interior of slots remains mutable
  end

  def inspect
    # process the slots first to ensure that nils are a single blank character
    # instead of "nil" or ""
    FORMAT % (self.slots.map { |v| v.nil? ? ' ' : v.to_s })
  end

  def [] (row, col)      = self.slots[self.index_for(row, col)]
  def []=(row, col, val) = self.slots[self.index_for(row, col)] = val

  def row(n)     = Row::new(self, n)
  def col(n)     = Col::new(self, n)
  def section(n) = Section::new(self, n)

  #
  # Returns a `Set` of the legal possibilities for the cell at the given row
  # and column. This method ignores the current contents of the cell. It only
  # performs a basic pass of the core rules of Sudoku; it does not attempt to
  # solve the puzzle in general and it does not consider whether other cells in
  # the same row, column, or section are themselves legal.
  #
  def possibilities(row, col)
    # we start assuming all numbers are possible, then remove the ones that
    # conflict
    possibilities = VALUES.dup
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
    SIZE.times.reduce(Set.new) do |indices, i|
      indices.merge self.row(i)    .incongruencies
      indices.merge self.col(i)    .incongruencies
      indices.merge self.section(i).incongruencies
    end
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
    self.slots.to_set.subset?(VALUES | [nil])
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

  protected

  attr_accessor :slots

  def index_for(row, col)
    row * SIZE + col
  end

  def section_of(row, col)
    (row / DIMENSION * DIMENSION) + (col / DIMENSION)
  end
end

class Row
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
    Sudoku::SIZE.times.map { |i| self[i] }
  end

  def incongruencies
    indices = Hash.new { |h,k| h[k] = [] }

    # we store a map of every value to the indices it's found in; congruent
    # values will only have *one* such index
    self.slots.each.with_index { |v, i| indices[v] << [row, i] }

    indices
      .reject { |k, v| k.nil? }       # not interested in nil values
      .select { |k, v| v.length > 1 } # not interested in unique values
      .map    { |k, v| v }            # we only need the indices themselves
      .reduce([], &:+)                # quick and dirty single-level flatten
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
    Sudoku::SIZE.times.map { |i| self[i] }
  end

  def incongruencies
    indices = Hash.new { |h,k| h[k] = [] }

    self.slots.each.with_index { |v, i| indices[v] << [i, col] }

    indices
      .reject { |k, v| k.nil? }
      .select { |k, v| v.length > 1 }
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
  FORMAT = [
    '┌───────┐',
    '│ %s %s %s │',
    '│ %s %s %s │',
    '│ %s %s %s │',
    '└───────┘',
  ].join("\n")

  def initialize(sudoku, index)
    self.sudoku = sudoku
    self.row    = (index / Sudoku::DIMENSION) * Sudoku::DIMENSION
    self.col    = (index % Sudoku::DIMENSION) * Sudoku::DIMENSION

    self.freeze
  end

  def [] (row, col)      = self.sudoku[self.row + row, self.col + col]
  def []=(row, col, val) = self.sudoku[self.row + row, self.col + col] = val

  def slots
    Sudoku::DIMENSION.times.map do |row|
      Sudoku::DIMENSION.times.map do |col|
        self[row, col]
      end
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
      .select { |k, v| v.length > 1 }
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
      Sudoku::SIZE.times do |i|
        Sudoku::SIZE.times do |j|
          assert_nil sudoku[i, j]
        end
      end
    end

    def test_indirectly_empty
      Sudoku::SIZE.times do |i|
        Sudoku::SIZE.times do |j|
          assert_nil sudoku.row(i)[j]
          assert_nil sudoku.col(j)[i]
          assert_nil sudoku.section(i)[j / 3, j % 3]
        end
      end
    end

    def test_unsolved
      refute_predicate sudoku, :solved?
    end

    def test_no_incongruencies
      assert_empty sudoku.incongruencies
    end

    def test_unlimited_possibilities
      Sudoku::SIZE.times do |i|
        Sudoku::SIZE.times do |j|
          assert_equal Sudoku::VALUES, sudoku.possibilities(i, j)
        end
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
        s.send(:slots).replace [
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
      Sudoku::SIZE.times do |i|
        Sudoku::SIZE.times do |j|
          assert_equal Set[sudoku[i, j]], sudoku.possibilities(i, j)
        end
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
end
