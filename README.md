# automata
This is a kind of generic FSA implementation.
It was written to handle keystrokes, so there are no accepting states but each transition can trigger actions:
- printing constant strings in the terminal: `print "hi!"`
- broadcasting the same kind of strings in UDP: `bc 2000 "HELLO EVERYONE"`

Combinators allow you prioritizing automata:
- `a < b` will produce `b`'s actions when `a` doesn't
- `a || b` will produce (at each step) actions from `a` and `b`, in this order

# Examples
## A cash register simulator
Running
```
./dot.byte cashregister.aut |dot -Tpng >cashregister.png
```
... on this file

    (* transition macros *)
    digit='0'-'9'

    (* action macros *)
    T=broadcast 2000 "tick"
    D=broadcast 2000 "ding"

    (* 0 is the initial state *)
    0:      digit -> typing; T

    typing: digit -> typing; T
            '\n' -> 0; D
... yields:

![Cash register graph](cashregister.png?raw=true)
