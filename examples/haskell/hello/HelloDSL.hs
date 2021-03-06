
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HelloDSL where

-------------------------------------------------------------------------------

-- grammar definition

-- simpleHello : start -> "hello()"
-- helloWithName : start -> "hello()" name
-- nameString : name -> "name(String)"

-------------------------------------------------------------------------------

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-------------------------------------------------------------------------------

-- AST nodes

data Start
  = SimpleHello
  | HelloWithName Name
  deriving (Show)

data Name
  = NameString String
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class NameTransition s t | s -> t where
  name :: String -> s -> t

class HelloTransition s t | s -> t where
  hello :: s -> t

class EndTransition s t | s -> t where
  end :: s -> t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev Start

data Node3 prev = Node3 prev Name

data Node4 prev = Node4 prev

data Node5 prev = Node5 prev String

-------------------------------------------------------------------------------

-- transition instances

instance EndTransition (Node2 prev) Start where
  end (Node2 _ arg1) = arg1

instance HelloTransition (Node1 prev) (Node4 (Node1 prev)) where
  hello src = Node4 src

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node3 (Node4 (Node1 prev))) t where
  end (Node3 (Node4 prev) arg1) = end (Node2 prev (HelloWithName arg1))

instance NameTransition (Node4 prev) (Node5 (Node4 prev)) where
  name arg1 src = Node5 src arg1

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node4 (Node1 prev)) t where
  end (Node4 prev) = end (Node2 prev (SimpleHello))

instance (EndTransition (Node3 (Node4 prev)) t) => EndTransition (Node5 (Node4 prev)) t where
  end (Node5 prev arg1) = end (Node3 prev (NameString arg1))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

