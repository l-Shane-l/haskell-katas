# Haskell Katas 🏋️‍♂️

## !!! A heavy work-in-progress/experimental project warning  !!! ##

### NOTE: Please be aware this is a brand new effort, most of the Katas are AI generated and a lot have not even been used or tested ###

## 🎯 Philosophy

This project is inspired by the concept of code katas - small, focused exercises that you repeat to internalize patterns and develop fluency. Just like learning a spoken language, a hypothetical path to mastering Haskell syntax by moving common patterns from conscious thought to automatic recall.

## 📚 Structure

The katas are organized into four progressive layers plus additional topics:

### Layer 1: Fundamentals

- **Pattern Matching** (`PatternMatching.hs`): Master pattern matching on core data types - lists, tuples, Maybe, Either
- **Exercises** (`Exercises.hs`): Classic beginner problems - FizzBuzz, factorial, fibonacci, currying, and recursion with accumulators

### Layer 2: Workhorse  

- **List Library Drills** (`ListLibraryDrills.hs`): Rapid recall exercises for map, filter, fold, zip, and other standard functions
- **List Comprehensions** (`ListComprehensions.hs`): From basic transformations to Pythagorean triples and nested comprehensions
- **List Patterns** (`ListPatterns.hs`): Common patterns like run-length encoding, chunking, frequency counting, and interleaving
- **Pattern Matching** (`PatternMatching.hs`): Advanced patterns with guards, as-patterns, nested patterns, and case expressions
- **Laziness** (`Laziness.hs`): Space leaks, infinite lists, lazy evaluation, and the Hamming sequence

### Layer 3: Toolkit

- **Data Structures** (`DataStructures.hs`): Choosing the right structure - when to use Map vs HashMap, List vs Vector vs Sequence, Text vs String

### Layer 4: Apex

- **Abstractions** (`Abstractions.hs`): Implement Functor for a Tree type and basic IO operations

### Data Types

- **Basic Types** (`BasicTypes.hs`): Product types, record syntax, smart constructors
- **Sum Types** (`SumTypes.hs`): Enums, sum of products, pattern matching on custom types
- **Recursive Types** (`RecursiveTypes.hs`): Binary trees, expression evaluators, tree traversals

### Type Classes

- **Basic Type Classes** (`BasicTypeClasses.hs`): Creating your own type classes (Reversible, Default)
- **Higher Kinded** (`HigherKinded.hs`): Type classes for type constructors (Compress, Filterable)
- **Newtype Instances** (`NewtypeInstances.hs`): Smart constructors, custom Num instances, validation
- **Type Applications** (`TypeApplications.hs`): Using TypeApplications extension for explicit type passing## 🚀 Getting Started

### Prerequisites

- GHC 9.2+ and Cabal 3.0+
- Or Stack (if you prefer)

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/haskell-katas.git
cd haskell-katas

# Build the project
cabal build

# Run the tests to see your progress
cabal test
```

### How to Practice

1. **Start Small**: Begin with Layer 1 exercises
2. **Implement Solutions**: Open the source files in `src/` and replace `undefined` with your implementations
3. **Run Tests**: Use `cabal test` to check your solutions
4. **Repeat**: The key to katas is repetition - try to solve them from memory
5. **Progress**: Move to the next layer once you're comfortable

### Example Workflow

The current workflow uses the `test/Spec.hs` file to control which tests run:

```haskell
-- In test/Spec.hs
import qualified Katas.Layer1_Fundamentals.PatternMatchingSpec
import qualified Katas.Layer1_Fundamentals.ExercisesSpec
import qualified Katas.Layer2_Workhorse.ListLibraryDrillsSpec
-- ... other imports

main :: IO ()
main = hspec $ do
  -- Comment/uncomment to control which tests run
  Katas.Layer1_Fundamentals.PatternMatchingSpec.spec
  Katas.Layer1_Fundamentals.ExercisesSpec.spec
  
  -- Katas.Layer2_Workhorse.ListLibraryDrillsSpec.spec  -- commented out
  -- Katas.Layer3_Toolkit.DataStructuresSpec.spec       -- commented out
```

Then run:

```bash
cabal test
```

This lets you focus on one module at a time by commenting out the others. Not the most elegant solution, but it works!

**Tip**: You can also run tests continuously while editing:

```bash
ghcid --command="cabal repl test:haskell-katas-test" --test="main"
```

## 📝 Project Status

This is an early experiment in creating effective Haskell practice materials. The current katas are LLM-generated and need refinement through actual use. Expect:

- A lot of change as it moves from a lot of AI generated content to more refined experience
- Some exercises to be replaced or rewritten
- New hand-crafted katas based on real needs
- Adjustments based on community feedback

## Nix support

Normally I provide nix flakes with my haskell project, but this is the first I've created on a macbook, because of the limited space I have on this laptop I run nix on it. So In the future I will likely add a flake but if anyone else would like to do that in the mean time I would be very grateful.

## 🔗 Resources

- [Code Kata Concept](http://codekata.com/)
- [Haskell Documentation](https://www.haskell.org/documentation/)

## 📄 License

MIT License - see LICENSE file for details
