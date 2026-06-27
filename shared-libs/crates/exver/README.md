# ExVer

`exver` — a SemVer extension with separate upstream and downstream versions, designed for package
distributors.

## Place in the monorepo

- Path: `shared-libs/crates/exver`.
- Cargo package: `exver` (directory name and package name match); crate-type `cdylib` + `rlib`.
- Consumers: `start-core` (`shared-libs/crates/start-core`) uses `Version`, `ExtendedVersion`, and
  `VersionRange` across package management, the registry, dependency resolution, and manifests.
- First-party: a direct path dependency (no registry crate, no `[patch]`). Also published to npm as
  `@start9labs/exver` from the wasm build.

This module was designed to address the problem of releasing updates to StartOS Packages where the upstream project was
either unaware of or apathetic towards supporting their application on the StartOS platform. In most cases, the original
package will support [semver2](https://semver.org/spec/v2.0.0.html). This leaves us with the problem where we would like
to preserve the original package's version, since one of the goals of the StartOS platform is transparency. However, on
occasion, a package's metadata or maintainer scripts may need to be updated without a corresponding update of the upstream
service, so, a downstream version has been added to indicate the version of the package wrapper for a given upstream version.

Additionally, we have decided to add a FLAVOR prefix to the version, indicating a version of a package that can be updated
to, but may not have an ordinal relationship with other packages of a different FLAVOR. This is useful for packages with
multiple forks.

## Usage

Add this to your `Cargo.toml`

```toml
[dependencies]
exver = "0.2.1"
```

## Operations

An `ExtendedVersion` contains 2-3 components: flavor (optional), upstream, and downstream. The upstream component should
follow SemVer semantics, however any number of digits is permitted. The downstream component follows identical semantics,
and is considered strictly less significant than the upstream version.

An `ExtendedVersion` can also be parsed from a string like `#flavor:0.1.2-beta.1:0`. They can also be serialized to strings.
The relevant parse function for `ExtendedVersion` is `ExtendedVersion::from_str`. It can be applied to a `&str` and will
produce a `Result<Version, ParseError>`.

The other half of this library deals with the type `VersionRange`. A `VersionRange` is a set that is either anchored at
a particular `ExtendedVersion` with some sort of comparison operator: `= >= <= > <` or it is described as a conjunction,
disjunction, or inversion of other `VersionRange`s. For convenience we also provide two constructors (`Any`, `None`) to
serve as identity elements on the `And` and `Or` constructors respectively. As a result, to gain maximum performance, you
should use the `and` and `or` smart constructors as opposed to their dumb counterparts `And` and `Or`. This will
immediately evaluate the identities and annihilators as opposed to building up the AST further, saving peak memory.

For convenience, there are two Monoid wrappers exposed: (`AnyRange`, `AllRange`). This allows you to `fold` an `Iterable`
with the `combine` operation seeded with the `empty` value. The semantic differences are whether or not `combine` uses
`and` or `or` respectively.

Most of the time you will want to parse these values from strings, but the internals are exposed for the rarer cases.
Some of the grammar from `semver` is supported (^1.2.3, ~2.3.4) as well.

Finally, the most useful operation in this package is the `satisfies` operation on `Version` with the argument of a
`VersionRange`. This is simply a predicate that tells you whether the `Version` falls inside the `VersionRange`.

## Laws

All laws listed below are equality of observation, not a literal `Eq` instance giving representational Equality. The
only observer that this library has is the `satisfies` operation. When you read "a === b", you should interpret that as
obs.satisfies(a) === obs.satisfies(b). These laws simply mean that it is always safe to do a substitution of a term on
the LHS for a term on the RHS without changing the meaning of your program.

- `And` is commutative: and(a,b) === and(b,a)
- `Or` is commutative: and(a,b) === or(b,a)
- `And` is associative: and(and(a,b),c) === and(a,and(b,c))
- `Or` is associative: or(or(a,b),c) === or(a,or(b,c))
- `Any` is identity of `And`: and(a, Any) === a
- `None` is identity of `Or`: or(a, None) === a
- `Any` annihilates `Or`: or(a, Any) === Any
- `None` annihilates `And`: and(a, None) === None
- `And` distributes over `Or`: and(a,or(b,c)) === or(and(a,b),and(a,c))
- `Or` distributes over `And`: or(a,and(b,c)) === and(or(a,b),or(a,c))
- `Not` follows DeMorgan's laws:
  - not(and(a, b)) === or(not(a), not(b))
  - not(or(a, b)) === and(not(a), not(b))

## Documentation

- [ARCHITECTURE.md](./ARCHITECTURE.md) — how the crate works internally (types, parsing,
  satisfiability, wasm surface).
- [CONTRIBUTING.md](./CONTRIBUTING.md) — toolchain, build/test, and PR conventions.
- [AGENTS.md](./AGENTS.md) — agent-facing rules and gotchas (`CLAUDE.md` is a one-line
  `@AGENTS.md` import).

Licensed under MIT (see `Cargo.toml`).
