# Character Class Glob Translation Design

## Goal

Resolve issue #6549 by making `directory.walk` translate glob character
classes into regular-expression character classes. Remove the `#6482` puzzle
after the behavior is covered by tests.

## Scope

The change is limited to `eo-runtime/src/main/eo/directory.eo`. It preserves
the existing recursive walk, brace-group translation, wildcard behavior, and
malformed-glob error handling. The symbolic-link and constant-stack-space
puzzles remain untouched.

## Translation

Extend the existing single-pass `translated` object with character-class
state. Outside a class, `[` opens the class. While the class is open:

- a leading `!` is emitted as `^`, providing glob negation semantics;
- range punctuation such as `-` keeps its regular-expression meaning;
- glob metacharacters such as `*`, `?`, `{`, and `}` are emitted as class
  characters instead of being translated as wildcards or brace groups;
- `]` closes the class and returns translation to its existing behavior.

An unmatched `[` remains unmatched in the generated regular expression, so
the existing compilation error continues to report a malformed glob.

## Tests

Use the embedded EO tests in `directory.eo` and follow red-green-refactor:

1. Add a failing test proving `[a-z]` matches a character in the range.
2. Add a failing test proving `[!a]` excludes `a` and accepts another
   character.
3. Add a failing test proving `[*]` treats the star literally.
4. Preserve the existing malformed `[` test.
5. Run the focused eo-runtime checks, formatting/lint checks available for the
   changed EO source, and inspect the final diff.

## Compatibility and Errors

No public object shape changes. Existing globs without character classes keep
their current translation. Regular-expression compilation remains the single
validation point and continues to produce the existing `Can't parse ... as a
glob pattern` error for malformed input.
