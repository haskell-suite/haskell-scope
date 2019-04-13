module Error1 where

-- Conflicting pattern bindings
x = () -- #1
x = () -- #2
y = x

-- Conflicting functions.
fn 1 = fn -- 'fn' inside the body refers to itself.
use_fn = fn -- Ambiguous.
fn 2 = fn

-- Conflicting data types
data X -- #1
data X -- #2

type Y = X

-- Conflicting type classes
class C a
class C b

-- Conflicting nested pattern
(z,z) = ()
