# Paul's Advent of Code Solutions
This is a repository of the various Advent of Code solutions that I've cooked up over the years (I started in 2021 but went back to previous years). There are partially complete solutions in there and some partially complete library code, so this is a work in progress. All code is written in Scala 2.13 in my personal style which tends to emphasize immutable object-oriented code over function applications like `flatMap()`.

## Structure
- `src`: The root of the source code
  - `src/common`: Common functionality shared across solutions
  - `src/grid`: A 2D grid library I wrote to cover common cases
  - `src/tyool20xx`: Solutions for the year 20*xx* (from 2015 to 2025) 
      - `src/tyool20xx/Main.scala`: The parent class for all the solutions for a given year
      - `src/tyool20xx/Dayn.scala`: The object implementing the solutions for day *n* of the calendar
- `input`: The root of the input tree, not shared due to the wishes of the puzzle creator. You will need to provide this separately
  - `input/tyool20xx`: Inputs for 20*xx*, generally of the form Day*n*.txt

**NOTE**: I needed a prefix since package names couldn't start with numbers and an affectation on certain forums was to refer to "The year of our Lord 2021" as in "Is this joke still used in the year of our Lord 2026?" So I went with the joke abbreviation that was commonly used, TYOOL.

**NOTE**: [src/common/Common.scala](src/common/Common.scala) should just be input functions but has grown to include other common functions that are inherited rather than imported. This needs to be cleaned up at some point.

## The `src/grid` Library
Since it's used so many places a short explanation of what `src/grid` is seems worthwhile. The `src/grid` library wraps a single-dimensional array of some arbitrary type to make it look like a 2D grid of that type. It comes with a variety of utility classes that are used throughout the project based on an old roguelike I had worked on years ago.

### `Grid[T]` and `Cell[T]`
A [`Grid[T]`](src/grid/Grid.scala) is a 2D grid of some type, either simple or complex. The simplest is generally a `Grid[Char]` which can be constructed from a `Seq[String]` provided by parsing one of the input files. It's common to want to refer to the coordinates of some point on a grid or its neighbors. In that case the data for a grid point can be wrapped with a [`Cell[T]`](src/grid/Cell.scala) which contains the value and other information. One small quirk: Cells are created on demand to save space in the underlying grid. They should be stack allocated in normal usage due to JVM escape analysis.

`Cell[T]` has a couple of operations that are a bit more complex than just getting the neighboring cells. `Cell.ray()` generates a ray in a direction, which is a lazy list of cells. Laziness allows for ring and torus-shaped grids to still work correctly with rays. `Cell.floodFill()()` takes a set of directions that it flood fills in, a boolean function whether a cell is a member of the flood filled set and produces a list of the cells visited in order. Finally, `Grid.mapCells()` takes a function from a `Cell[T]` to some type `R` and generates a new `Grid[R]` of the result type.

Normally going off an edge causes an error. `EdgeBehavior` can be set at grid creation time to indicate that one or both of the directions wrap, so grid coordinates are actually calculated on a modular field. This allows for ring or torus shaped grids as necessary.

### `Point`
Used throughout the project a [`Point`](src/grid/Point.scala) is a point in two-dimensional space with an integer x and y coordinate. Anything can implement the trait by having an `x()` and `y()` or they can use `Point(x, y)` to generate an instance at a given spot. Importantly a `Cell[T]` implements `Point`. Any two points can be compared for distance using a variety of metrics.

### `Direction` and `DirectionSet`
Cells can have neighbors in eight directions, and it's common to perform different behavior in only some directions, such as the four cardinal directions. This enumeration for the directions is usable outside the grids such as if a problem requires walking city blocks but there is no data associated with each city block. The `DirectionSet` class is admittedly overengineered, packing the enumeration values into an integer to save space. It was a proof of concept to see what such a thing would look like.

### `Bearing` and `BearingSet`
A subtly different abstraction I felt worth distinguishing in the library is bearings. A bearing is a relative direction, so left when facing north is west. This allows for things like maze crawlers to be written more naturally since they can just turn left from their current direction instead of having a switch statement or pattern matching. Like `DirectionSet`, `BearingSet` is a compact representation of bearings as a single integer.