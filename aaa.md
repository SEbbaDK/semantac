```
category Nat = "natural number"
category Ident = "identifier"
category Exp = Nat ∪ Ident ∪ Syntax
category Env = Ident -> Nat

system <Exp, Env> => <Exp, Env>
```



```
Lamda:
<("λ" a "." b) c , d> => <b , d[a |-> c]>
```
- depth-0:             `<_ _ , _> => <_ , _>`
- depth-*: `<("λ" _ "." _) _ , _> => <_ , _>`



```
Var:
<a , b[a |-> c]> => <c , d[a |-> c]>
```
- depth-*: `<_ , _> -=> <_ , _>`

This method would have the `Var` be self-recursive, which technically it could be if `c` itself is a variable.
If on the other hand we infer the types and use the type signatures, then we get the following.
- depth-*: `<Ident , Env> -> <Nat , Env>`

Thus by using types we can reduce the over approximation of the call graph, allowing more correct programs through.
It should be noted that this only works because `Env` is designed to not be cyclic by restricting the inputs and outputs to types that are not type-compatible with eachother.
It might however be desirable to define an `Env` that maps to `Exp` instead of to `Nat`.
This would allow the `Env` to represent cyclic mappings which would allow `Var` to be non-terminating.  



```
If-1:
"if" ("true") "then" b "else" c => b
```
- depth-*: `"if" "true" "then" b "else" _ => b`
- types: `Syntax => Syntax`
- it is recursive with itself but its arguments are decreasing


```
If-2:
"if" ("false") "then" b "else" c => c
```
- depth-*: `"if" "false" "then" _ "else" _ => _`
- types: `Syntax => Syntax`
- it is recursive with itself but its arguments are decreasing


```
While:
"while" a "do" b => "if" a "then" (b ";" ("while" a "do" b)) "else" ("skip")
```
- depth-0: `"while" _ "do" _ => "if" _ "then" _ "else" _`
- depth-1: `"while" _ "do" _ => "if" _ "then" (_ ";" _) "else" ("skip")`
- depth-*: `"while" _ "do" _ => "if" _ "then" (_ ";" ("while" _ "do" _)) "else" ("skip")`
- it is recursive with itself and it is _not known_ if it is decreasing.
