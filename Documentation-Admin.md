## Needs Documentation

Add topics you'd like documented, followed by specific questions about the topic.

- Newtype
-- Difference between keyword and type class.
-- Removed `runAdditive` in v0.10, moved to `wrap` from `Newtype` class. Why have `run*` in first place?

- Functional Dependencies
-- What type-checking errors does it resolve?
-- When should I use functional dependency in my type class?

- Typed-holes and type wildcards
-- Names are similar, needs clarification.

- Compile Iteration Techniques
-- Hole-driven development with typed-holes.
-- Faculties of editor plugins. Using pscid as editor-independent alternative.
-- Iterating module while testing in psci.

## Documentation System

We currently use GitHub Wiki to host documentation pages because it was simple to start and the documentation needs of a young project are few. We would like to use a file-based documentation system, such as the one used by ReadTheDocs, for many reasons: 1) PR-based doc updates imply peer-review, which inspires belief that content is correct, 2) documentation is versioned with the code, which enables users of any version of the compiler to find valid documentation, 3) allow international users to submit translations.

