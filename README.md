# mighty.standards

Standard components for the mighty framework.

## Repository structure

```
mighty.standards/
├── components/
│   └── {name}/
│       ├── {name}.mustache
│       └── test-{name}.R
├── DESCRIPTION
└── .github/
    ├── ISSUE_TEMPLATE/
    ├── pull_request_template.md
    └── workflows/
```

- `components/` — each component lives in its own directory containing a mustache template and a test file.
- `DESCRIPTION` — infrastructure dependencies (`covr`, `testthat`, `mighty.component`) go under `Suggests`. Packages used in component code go under `Imports`.
- `.github/` — issue templates, PR template, and CI workflows.

## CI workflow

The `test-components` workflow runs on:

- Push and PR to `main`
- Push and PR to `dev/**` branches (for staging draft components)

## Running tests locally

Install dependencies:

```r
pak::pak()
```

Run all component tests:

```r
dirs <- list.dirs("components", recursive = FALSE)
lapply(dirs, testthat::test_dir, stop_on_failure = TRUE)
```

Run tests for a single component:

```r
testthat::test_dir("components/dummy")
```
