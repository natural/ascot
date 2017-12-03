### natural.ascot

A Clojure library designed to provide specs from JDBC relations.

#### Usage

```lisp
(require 'natural.ascot)
(-> (natural.ascot/tables db-spec) (natural.ascot/register :awesome-tables))
```

#### License

Copyright © 2017 Troy Melhase

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
