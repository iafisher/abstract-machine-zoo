{-
- The CEK machine is an abstract machine that evaluates the simple untyped lambda calculus. Its
- name is an acronym for the three elements that comprise its state: C for the lambda expression
- to be evaluated, E for the environment, and K for the continuation. The environment represents
- the bindings of variable names. The continuation is used to implement control flow.
-
- Author:  Ian Fisher (iafisher@protonmail.com)
- Version: April 2019
-}
import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup)
import qualified Data.Map as Map

-- A CEK machine consists of an expression to be evaluated, an environment to evaluate it in,
-- and a continuation to execute next.
data CEKMachine = CEKMachine {
    expr :: Term,
    env :: Env,
    kont :: Continuation
} deriving (Show)

-- A term in the simple lambda calculus is either a symbol, an application, or an abstraction.
data Term = Sym Symbol | App Term Term | Lambda Symbol Term deriving (Show)

-- The CEK machine only needs three kinds of continuations:
--   Ar, for evaluating the argument of an application next
--   Fn, for evaluating the body of a function next
--   Halt, for halting computation
data Continuation = Ar Term Env Continuation | Fn Term Env Continuation | Halt deriving (Show)

type Symbol = String
type Value = (Term, Env)
data Env = Env (Map Symbol Value) deriving (Show)


-- Step the machine once.
step :: CEKMachine -> CEKMachine
step machine = case expr machine of
    -- If we have a symbol, we just look it up in the environment.
    Sym s -> case envLookup s (env machine) of
        Just (t, env0) -> CEKMachine { expr = t, env = env0, kont = kont machine }
        Nothing -> error("undefined symbol")
    -- If we have an application, we get ready to evaluate the function to be applied first, and
    -- we set up an Ar continuation to evaluate the argument next.
    App e0 e1 -> CEKMachine {
        expr = e0,
        env = env machine,
        kont = Ar e1 (env machine) (kont machine)
    }
    v -> case kont machine of
        -- If we have an Ar continuation, that means we are in the process of evaluating an
        -- application, and we've just finished evaluating the function to be applied. Therefore,
        -- we should evaluate the argument next (which is the first field of the continuation),
        -- and set the continuation to be Fn.
        Ar e env0 kont -> CEKMachine {
            expr = e,
            env = env0,
            kont = Fn v env0 kont
        }
        -- If we have an Fn continuation, that means we are in the process of evaluating an
        -- application, and we've just finished evaluating the argument. Therefore, we should
        -- evaluate the body of the function next, with the parameter bound to the value of the
        -- argument.
        Fn (Lambda x body) env0 kont -> CEKMachine {
            expr = body,
            env = envInsert x (v, env machine) env0,
            kont = kont
        }
        Halt -> machine

-- Step the machine until it reaches a halt configuration.
stepUntilHalt :: CEKMachine -> CEKMachine
stepUntilHalt m@(CEKMachine { expr = Lambda _ _, kont = Halt }) = m
stepUntilHalt m = stepUntilHalt (step m)

-- Utility functions for working with Env objects
envLookup :: Symbol -> Env -> Maybe Value
envLookup s (Env map) = lookup s map

envInsert :: Symbol -> Value -> Env -> Env
envInsert s v (Env map) = Env (insert s v map)

-- (λx.x λy.y)
t = App (Lambda "x" (Sym "x")) (Lambda "y" (Sym "y"))
main = putStrLn (show (stepUntilHalt CEKMachine { expr = t, env = Env empty, kont = Halt }))
