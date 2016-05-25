## Master
[![Travis-CI Build Status](https://travis-ci.org/drackham/dcmdata.svg?branch=master)](https://travis-ci.org/drackham/dcmdata)

## Develop
[![Travis-CI Build Status](https://travis-ci.org/drackham/dcmdata.svg?branch=develop)](https://travis-ci.org/drackham/dcmdata)
[![codecov.io](https://codecov.io/github/drackham/dcmdata/coverage.svg?branch=develop)](https://codecov.io/github/drackham/dcmdata?branch=develop)

## Synopsis

This R package manages the heavy lifting of keeping track of the various data generation algorithms that I am using to do simulation work on CDA models. Currently there are data generation algorithms (in various states of functionality!) for:
* R-DINA with a simple 2 attribute Q-matrix
* R-DINA with the Hartz Roussos Q-matrix (low)
* RUM Hartz Roussos with Q-matrix (low)
* R-RUM Hartz Roussos with Q-matrix (low)

Included Q-matrices are:
* Simple Q-matrix (2 attribute, 30 item) 
* Hartz Roussos Q-matrix (Low cognitive complexity; 7 attribute, 40 item)

This is very much a work in progress!  The goal is to have fully functional algorithms that are fully tested using testthat. The current code coverage levels may or may not indicate completeness/quality of tests (yet).

## Code Example

q <- hartzRoussosQ()
data <- rDINASimpleQ(500)

## Motivation

Simulation work is hard enough without having to worry about managing and testing the data simulation algorithms.

## Installation

Install using `devtools` using the repo URL.

## Tests

Test run as part of a CI environment using testthat.

## Contributors

Dave Rackham ddrackham@gmail.com

## License

MIT License (see License.txt)
