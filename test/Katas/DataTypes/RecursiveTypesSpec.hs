module Katas.DataTypes.RecursiveTypesSpec (spec) where

import Katas.DataTypes.RecursiveTypes
import Test.Hspec

spec :: Spec
spec = do
  describe "Binary Tree operations" $ do
    let sampleTree = Branch (Branch Leaf "left" Leaf) "root" (Branch Leaf "right" Leaf)

    it "pretty prints a string tree" $
      showStringTree sampleTree `shouldBe` "(left) root (right)"

    it "adds elements to maintain BST property" $ do
      let tree = addElementToIntTree Leaf 5
          tree' = addElementToIntTree tree 3
          tree'' = addElementToIntTree tree' 7
      doesIntExist tree'' 3 `shouldBe` True
      doesIntExist tree'' 5 `shouldBe` True
      doesIntExist tree'' 7 `shouldBe` True
      doesIntExist tree'' 4 `shouldBe` False

    it "finds existing elements" $
      let tree = Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
       in doesIntExist tree 2 `shouldBe` True

    it "returns false for non-existing elements" $
      doesIntExist Leaf 5 `shouldBe` False

    it "calculates tree size" $ do
      treeSize Leaf `shouldBe` 0
      treeSize sampleTree `shouldBe` 3

    it "calculates tree depth" $ do
      treeDepth Leaf `shouldBe` 0
      treeDepth (Branch Leaf 1 Leaf) `shouldBe` 1
      treeDepth sampleTree `shouldBe` 2

    it "converts tree to list in-order" $
      treeToList (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf))
        `shouldBe` [1, 2, 3]

  describe "Expression evaluation" $ do
    it "evaluates literals" $
      eval (Lit 42) `shouldBe` 42

    it "evaluates addition" $
      eval (Add (Lit 5) (Lit 3)) `shouldBe` 8

    it "evaluates complex expressions" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Sub (Lit 10) (Lit 6))) `shouldBe` 20

    it "handles division" $
      eval (Div (Lit 10) (Lit 2)) `shouldBe` 5

  describe "Safe expression evaluation" $ do
    it "safely evaluates valid expressions" $
      safeEval (Add (Lit 5) (Lit 3)) `shouldBe` Right 8

    it "returns error for division by zero" $
      safeEval (Div (Lit 10) (Lit 0)) `shouldBe` Left "Error: division by zero"

    it "propagates errors in nested expressions" $
      safeEval (Add (Lit 5) (Div (Lit 10) (Lit 0)))
        `shouldBe` Left "Error: division by zero"

  describe "Expression pretty printing" $ do
    it "prints simple expressions" $
      prettyPrint (Add (Lit 5) (Lit 10)) `shouldBe` "5 + 10 = 15"

    it "prints nested expressions with parentheses" $
      prettyPrint (Mul (Lit 2) (Add (Lit 3) (Lit 4)))
        `shouldBe` "2 × (3 + 4) = 14"

    it "uses division symbol" $
      prettyPrint (Div (Lit 10) (Lit 2)) `shouldBe` "10 ÷ 2 = 5"
