{-
- The CESK machine is an extension of the CEK machine that adds a mutable store. Like the CEK
- machine, the CESK machine also evaluates the simple untyped lambda  calculus.
-
- Author:  Ian Fisher (iafisher@protonmail.com)
- Version: April 2019
-}
import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup, size)
import qualified Data.Map as Map

-- A CESK machine consists of an expression to be evaluated, an environment to evaluate it in,
-- a mutable store, and a continuation to execute next.
data CESKMachine = CESKMachine {
    expr :: Term,
    env :: Env,
    store :: Store,
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
type Address = Integer
type Store = Map Address Value
type Env = Map Symbol Address


-- Step the machine once.
step :: CESKMachine -> CESKMachine
step machine = case expr machine of
    -- If we have a symbol, we just look it up in the environment.
    Sym s -> case lookup s (env machine) of
        Just addr -> case lookup addr (store machine) of
            Just (t, env0) -> CESKMachine {
                expr = t, env = env0, store = store machine, kont = kont machine
            }
            Nothing -> error("undefined symbol")
        Nothing -> error("undefined address")
    -- If we have an application, we get ready to evaluate the function to be applied first, and
    -- we set up an Ar continuation to evaluate the argument next.
    App e0 e1 -> CESKMachine {
        expr = e0,
        env = env machine,
        store = store machine,
        kont = Ar e1 (env machine) (kont machine)
    }
    v -> case kont machine of
        -- If we have an Ar continuation, that means we are in the process of evaluating an
        -- application, and we've just finished evaluating the function to be applied.
        -- Therefore, we should evaluate the argument next (which is the first field of the
        -- continuation), and set the continuation to be Fn.
        Ar e env0 kont -> CESKMachine {
            expr = e,
            env = env0,
            store = store machine,
            kont = Fn v env0 kont
        }
        -- If we have an Fn continuation, that means we are in the process of evaluating an
        -- application, and we've just finished evaluating the argument. Therefore, we should
        -- evaluate the body of the function next, with the parameter bound to the value of the
        -- argument.
        Fn (Lambda x body) env0 kont -> CESKMachine {
            expr = body,
            env = insert x (newAddress (store machine)) env0,
            store = insert (newAddress (store machine)) (v, env machine) (store machine),
            kont = kont
        }
        Halt -> machine

-- Step the machine until it reaches a halt configuration.
stepUntilHalt :: CESKMachine -> CESKMachine
stepUntilHalt m@(CESKMachine { expr = Lambda _ _, kont = Halt }) = m
stepUntilHalt m = stepUntilHalt (step m)

-- Allocate a new address to be used for the store.
newAddress :: Store -> Address
newAddress s = toInteger (size s)

-- (λx.x λy.y)
t = App (Lambda "x" (Sym "x")) (Lambda "y" (Sym "y"))
main = putStrLn (show (stepUntilHalt CESKMachine { expr = t, env = empty, store = empty, kont = Halt }))
