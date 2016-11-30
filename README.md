# marchio

[CommonMark](http://commonmark.org/) parser for Clojure/Script.

TODO

[![Dependency Status](https://www.versioneye.com/user/projects/57df6c1b037c2000458f785a/badge.svg?style=flat-square)](https://www.versioneye.com/user/projects/57df6c1b037c2000458f785a)

## Rationale

TODO add links to Monadic Parsing and Parsing with Derivatives papers.

Instaparse is very powerful and well done, but due to its tradeoffs it [starts having issues around 20k-200k chars](https://github.com/Engelberg/instaparse/issues/131#issuecomment-209204750) (one of the test files is 10M, Commonmark C implementation eats it in couple of seconds).

So, one alternative would be to implement a small parser combinators library (for fun) or use one of the following:
- https://github.com/youngnh/parsatron
- https://github.com/blancas/kern
- https://github.com/van-clj/zetta-parser

Kern looks best.

## Usage

TODO

## Roadmap

- [ ] Pass all the spec test cases
- [ ] Handle pathological inputs: markdown-it test suite
- [ ] ClojureScript port

## Contributing

### Clone

Clone with `git clone --recursive git://github.com/ff-/marchio`.  
This will pull in the folder `spec`the 
[last CommonMark specification](https://github.com/jgm/CommonMark). 

### Testing

TODO:
`python3 spec/test/spec_tests.py --program $PROG`
or
`python3 spec/test/spec_tests.py --dump-tests`

### Running

TODO: `lein run`?

## Internals

TODO: describe namespaces, where to find stuff, datastructures

## License

Copyright © 2016 Fabrizio Ferrai and 
[contributors](https://github.com/ff-/marchio/graphs/contributors)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
