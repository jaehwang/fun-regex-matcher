# Functional Regualar Expression Matcher #

We implement three kinds of regular expression matchers:

* Functional matcher with backtracking
* Thompson NFA Simulation (no backtracking)
* CPS version in Harper's "Proof-directed debugging"

## Development ##

We use Maven.

* mvn scala:cctest -Dfsc=false

### Testing ###

* mvn test

### Running ###

* mvn scala:run -Dlauncher=...
