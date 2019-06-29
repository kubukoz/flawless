# Flawless design goals

- Purely functional testing ✅
  - No side-effectful registration ✅
  - No reflection or a predefined runtime/runner ✅

- Thin DSL ✅
  - only a few operators in itself ✅
  - combining tests using Cats operators when convenient, providing necessary Cats instances ✅

- Great UX
  - Readable, instantaneous output ❌ with colors ✅, pretty printing ✅ (custom typeclass defaulting to cats.Show impl? ❌), file paths ✅

- Ran as an app and not using a test runner ✅
  - works with fury by default ✅
  - could be adapted to sbt's if need be ✅

- Future: extensibility
  - handle tests as values ✅, inspect them ✅
  - maybe add retries, timeouts, fallbacks on assertions from the outside...
  - plugins as wrappers over tests, e.g. flaky test finding etc...

- Configuration through command-line parameters (?)
  - only
  - except
