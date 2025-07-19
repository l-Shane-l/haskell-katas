# Simple Main.hs

```haskell
module Main where

-- Import kata modules as you work on them
-- import Katas.Lists.BasicOperations

main :: IO ()
main = putStrLn "Run 'cabal test' to see the kata tests!"
```

# Updated README section with commands to copy/paste

```markdown
## Running Tests

### Quick Test Commands (copy and paste)

**List Katas:**
```bash
# All tests
cabal test

# Specific kata tests
cabal test --test-option="--match=/BasicOperations/"
cabal test --test-option="--match=/FoldingFundamentals/"
cabal test --test-option="--match=/ListBuilding/"
cabal test --test-option="--match=/Sublists/"
cabal test --test-option="--match=/Searching/"
cabal test --test-option="--match=/Zipping/"
cabal test --test-option="--match=/AdvancedFolds/"
cabal test --test-option="--match=/ListComprehensions/"

# Specific function tests
cabal test --test-option="--match=/myMap/"
cabal test --test-option="--match=/myFoldl/"
```

**Maybe/Either Katas (when added):**

```bash
cabal test --test-option="--match=/MaybeChains/"
cabal test --test-option="--match=/EitherValidation/"
cabal test --test-option="--match=/MaybeEitherConversion/"
```

**Function Katas (when added):**

```bash
cabal test --test-option="--match=/Composition/"
cabal test --test-option="--match=/Currying/"
cabal test --test-option="--match=/HigherOrder/"
```

**Type Katas (when added):**

```bash
cabal test --test-option="--match=/AlgebraicTypes/"
cabal test --test-option="--match=/RecordSyntax/"
cabal test --test-option="--match=/NewtypePatterns/"
```

### Shell Aliases (Optional)

Add to your `.bashrc` or `.zshrc` for quick access:

```bash
# General
alias hkt='cabal test'

# List katas
alias hk-basic='cabal test --test-option="--match=/BasicOperations/"'
alias hk-fold='cabal test --test-option="--match=/FoldingFundamentals/"'
alias hk-build='cabal test --test-option="--match=/ListBuilding/"'
alias hk-sublist='cabal test --test-option="--match=/Sublists/"'
alias hk-search='cabal test --test-option="--match=/Searching/"'
alias hk-zip='cabal test --test-option="--match=/Zipping/"'
alias hk-advanced='cabal test --test-option="--match=/AdvancedFolds/"'
alias hk-comp='cabal test --test-option="--match=/ListComprehensions/"'
```

Then just use: `hk-fold` to run folding tests, etc.

```
# haskell-katas
