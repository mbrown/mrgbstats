# mrgbstats

A Clojure library for basic statistical analysis using permutation testing and
Benjamini-Hochberg false discovery rate (FDR) correction for multiple
comparisons

Original author: Matthew Robert Graham Brown


## Version

Current version: 0.1.0


## Dependencies

random-seed version 1.0.0


## Installation

Clone mrgbstats from the Github repository:

```
git clone https://github.com/mbrown/mrgbstats
```

Install mrgbstats locally in your Maven .m2 folder, using leiningen:

```
cd mrgbstats
lein pom; lein jar; lein install
```

or using boot:

```
cd mrgbstats
boot pom; boot jar; boot install
```

## Usage

In your project.clj file, add dependency:

```clojure
[mrgbstats "0.1.0"]
```

Clojure code:

```clojure
(require '[mrgbstats.core :as mrgbstats])
(mrgbstats/permtest a b numiter) ; a, b = vectors of values; numiter = integer
(mrgbstats/fdr pvec) ; pvec = vector of p-values
```


## Examples

```clojure
(require '[mrgbstats.core :as mrgbstats])
(mrgbstats/permtest [3 2 3 2 3 3 2 1 2 3] [5 4 2 4 2 7 3 5 6] 1000)
(mrgbstats/fdr [0.1 0.035 0.042 0.04 0.01 0.45 0.8 0.001 0.02 0.01 0.008])
```


## License

Copyright © Matthew Robert Graham Brown 2019

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

