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
  | Var v REQUIRES Set t
  | Var v PERMITS  Set t
  | Arg (Var v),(Int n) REQUIRES Set t
  | Arg (Var v),(Int n) PERMITS  Set t

Var = <symbol>

Arg = Var,Int

Set = (Var | Arg | Type) list



```

