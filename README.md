[![Build Status](https://travis-ci.org/jaehwang/fun-regex-matcher.png)](https://travis-ci.org/jaehwang/fun-regex-matcher)

# Functional Regular Expression Matcher for Fun #

We implement three kinds of regular expression matchers:

* Functional matcher with backtracking
* Thompson NFA Simulation (no backtracking)
* CPS version in Harper's "Proof-directed debugging"

## Development ##

We use Maven.

* mvn scala:cctest -Dfsc=false

## Testing ##

* mvn test

## Running ##

* mvn scala:run -Dlauncher=...

## Sample Apps ##

### My "really" mini grep ###

* mvn scala:run -Dlauncher=grep -DaddArgs="arg|...|arg"

`mvn assembly:assembly` creates a executable jar.
