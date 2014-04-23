Type inference thingy for Scheme
================================

Primitive Types
---------------
```
*any*
boolean
number
char
string
symbol
pair
```

Complex Types
-------------
```
function =
  *list-of-arg-types*->*return-type*
```

Constraints
-----------
Flat sets
don't worry, for now, about conditionals
```
Constraint =
  | Var v REQUIRES Type_reference t
  | Var v PERMITS  Type_reference t
  | Arg (Var v),(Int n) REQUIRES Type_reference t
  | Arg (Var v),(Int n) PERMITS  Type_reference t

Var = <symbol>

Arg = Var,Int

Type_reference = (Var | Arg | Type)


```
What this means: Internally, we are for each variable v keeping track of a
"Type", which corresponds to the universe U_v of values that "could be" taken
on by v.  Let t be a type.  When we see a constraint of the form v REQUIRES t,
we are saying:

U_v <= t.

When we see a constraint of the form v PERMITS t, we are saying:

(U_v intersect t) is nonempty.

Now suppose t is some kind of Type_reference other than a type.  Thus t is
either a variable or an argument.  Then we think of "v REQUIRES t" as saying

U_v <= U_t.

_Note on variables_: A "variable" here is either a variable that occurs in the
code being analyzed, or an internal identifier for a procedure (we might need
names for lambdas which are anonymous in the code).  We keep a unique
identifier for each lambda so that the arguments to the lambda can be
referenced.


