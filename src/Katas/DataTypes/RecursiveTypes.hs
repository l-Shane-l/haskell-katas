module Katas.DataTypes.RecursiveTypes where

-- | Binary tree definition
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

-- | Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree = undefined

-- | Add a new integer into a binary tree of integers (BST style)
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree = undefined

-- | Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist = undefined

-- | Count nodes in tree
treeSize :: BinaryTree a -> Int
treeSize = undefined

-- | Get tree depth
treeDepth :: BinaryTree a -> Int
treeDepth = undefined

-- | Convert tree to list (in-order traversal)
treeToList :: BinaryTree a -> [a]
treeToList = undefined

-- | Simple expression type
data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

-- | Evaluate an expression
eval :: Expr -> Int
eval = undefined

-- | Safe evaluation that handles division by zero
safeEval :: Expr -> Either String Int
safeEval = undefined

-- | Pretty print an expression
prettyPrint :: Expr -> String
prettyPrint = undefined
