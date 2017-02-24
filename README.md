Clojure's default data structures are great, but sometimes the problems we're trying to solve call for something else.  However, alternate data structures should still behave as much as possible like vectors, sets, or maps.

This library tests whether a given data structure is equivalent to whatever data structure it mimics, using [test.check](https://github.com/clojure/test.check).  This is both a validation that the data structure is correct and that it implements all necessary interfaces.  

The methods exposed here are compatible with ClojureScript.

### usage

Since this is only for testing, it should be added as a `:dev` dependency:

```clj
{:profiles {:dev {:dependencies [[collection-check "0.1.7"]]}}}
```

To validate a vector-like data structure, you can use `(assert-vector-like empty-collection element-generator)`.  For instance:

```clj
> (require '[collection-check :refer :all])
nil
> (assert-vector-like [] simple-check.generators/int)
true
```

We can also specify the number of tests we'd like to perform, which defaults to `1000`:

```clj
> (assert-vector-like 1e5 [] simple-check.generators/int)
true
```

If the assertion fails, it will throw an exception describing the custom and reference collections, and the actions that were taken to reproduce this error:

```clj
 java.lang.Exception: Assert failed: (= a b)
  a = #{0 9 -10}
  b = #{0 9 -10}
  actions = (-> coll transient (conj! -10) persistent! transient (conj! 9) persistent! transient (disj! -10) persistent! (conj -10))
```

We can do the same for sets via `assert-set-like`.  For maps, `assert-map-like` takes two generators, one for keys, and another for values.

### license

Copyright © 2017 Zachary Tellman

Distributed under the MIT License.
