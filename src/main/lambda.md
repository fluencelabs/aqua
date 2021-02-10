data structures consist of:

```
scalar types
records[type]
arrays[type]
```

From lambda, we want to have a non-destructive transitions.

Assume we have variables a...z in scope.

Lambda is:

```
lambda = \x -> y

lambda app = a..z lambda

.smth = \x -> x.smth
[lambda] = \x -> for y in x do y lambda
.0 = \x -> take[0] of x
```

To check lambda, we color the trace (all touched data). It should be keeped (not GC).

What we want:
- Get data from variable, derive its type
- Check if variable has a certain feature (map to bool)
- Convert a tuple to smth (check for inclusion)
  
union: union(A, B)
intersection: inter(A, B)

subtraction: sub(A, B)

partition: part(A, pred)

product (?): prod(A, B)

is_empty
ord:: a, a -> -1, 0, 1

inclusion: contains(A, a)

addition with respect to order: add(A, a, ord)
order: order(A, ord)

reducer: e.g. size?
take: lim(A, int) (can be done with partition, if has access to index)

- Intersection, is_empty, inclusion, difference  -- especially important for streams
- Order a stream, e.g. by reducing into a new stream (re canonicalize)