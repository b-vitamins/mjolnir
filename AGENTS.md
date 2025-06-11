# Development Guidelines for Mjolnir

This file defines the practices that automated agents must follow when working in
this repository.

## Commit Message Standards
- Use [Conventional Commits](https://www.conventionalcommits.org/).
- Structure: `<type>(<scope>): <description>` where `scope` is optional.
- Types used in this project: `feat`, `fix`, `chore`, `docs`, `refactor`, `test`.
- Example: `feat: add window cycling command`.

## Commit Sequencing Guidelines
- Keep commits atomic and logically grouped.
- Separate refactors from functional changes.
- Update or add tests in the same commit as the code change when applicable.

## Pull Request Standards
- Title should follow Conventional Commits style summarising the change.
- Description must include `Summary`, `Testing`, and `Future Work` sections.
- Ensure all pre-commit hooks pass before marking ready for review.

## Code Housekeeping Protocols
- Keep dependencies up to date when security issues arise.
- Remove obsolete code and files during refactors.
- Record major architectural decisions in `docs/adr/` when applicable.

## Architecture and Design Guidelines
- Prefer small functions with lexical binding.
- Public APIs should be documented with docstrings.
- Avoid introducing external dependencies without discussion.

## Pre-commit Checks Configuration
- Pre-commit is configured via `.pre-commit-config.yaml`.
- Hooks executed:
  - `trailing-whitespace`
  - `end-of-file-fixer`
  - `check-added-large-files`
  - `elisp byte compile` which runs `emacs --batch -Q -L . -f batch-byte-compile mjolnir.el`.
- Run `pre-commit run --all-files` before committing.

## Version Management
- The project follows [Semantic Versioning](https://semver.org/).
- Update `CHANGELOG.md` under `Unreleased` with each meaningful change.
- Tags use the format `vMAJOR.MINOR.PATCH`.

## CHANGELOG Maintenance Protocol
- Categorize entries under Added, Changed, Deprecated, Removed, Fixed, or Security.
- Keep an `Unreleased` section at the top.
- Link issues or PRs when relevant.

## Testing Standards
- Unit tests use Emacs' built-in `ert` framework when present.
- Place future tests in the `test/` directory.
- All tests must pass before merging.

## Documentation Standards
- Update `README.md` when user-facing behavior changes.
- Keep function docstrings current.

## Project-Specific Guidelines
- Require Emacs 25.1 or newer.
- Byte compile `mjolnir.el` as part of pre-commit.
