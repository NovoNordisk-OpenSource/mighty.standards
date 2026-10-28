## Bug Report

### Description

A global filter can depend on a column from another domain (for example `SV.VISIT`). mighty joins that column onto the identity domain (aka the ADaM domain being produced)  before the filter runs. If the identity domain already has a column with the same name (for example `VISIT` copied from the base domain), the join gives two columns with the same name. `dplyr::left_join()` renames them to `VISIT.x` and `VISIT.y`, and the filter expression cannot find `VISIT`.

mighty generates the code without an error. The error occurs only when the generated program runs.

The spec cannot be correct as written: the filter expression uses the bare name `VISIT`, so it does not say which of the two columns it means. mighty must find this when it validates the spec.

### Reproducible example

`_mighty.yml`:

```yaml
external_data:
  - id: SV
    keys: [STUDYID, USUBJID, VISITNUM]
repos:
  - "github::NovoNordisk-OpenSource/mighty.standards/components"
```

ADLB spec:

```yaml
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [STUDYID, USUBJID, LBSEQ]

population:
  base:
    - domain: LB
      filter: NA
      depends:
        - NA
  global:
    - filter: VISIT == 'BASELINE'
      depends:
        - SV.VISIT

columns:
  - id: STUDYID
  - id: USUBJID
  - id: LBSEQ
  - id: VISITNUM
  - id: VISIT
```

```r
res <- mighty::generate_adam_code(
  adam_specifications = "spec",
  path_connector_config = "_connector.yml"
)
```

The generated init and filter steps are:

```r
# ADLB-init_domain -------------------------------------------------------------
ADLB <- LB |>
    dplyr::select(STUDYID,USUBJID,LBSEQ,VISITNUM,VISIT) |>
    admiral::convert_blanks_to_na()

# ADLB-filter_domain -----------------------------------------------------------
ADLB <- ADLB |>
  dplyr::left_join(SV |> dplyr::select(VISIT,STUDYID,USUBJID,VISITNUM),
                   by = dplyr::join_by(STUDYID,USUBJID,VISITNUM), relationship = "many-to-one")

ADLB <- ADLB |>
  dplyr::filter(VISIT == 'BASELINE')

ADLB <- ADLB |>
  dplyr::select(STUDYID,USUBJID,LBSEQ,VISITNUM,VISIT)
```

Run these steps on test data:

```r
LB <- data.frame(STUDYID = "S1", USUBJID = "01", LBSEQ = 1:2, VISITNUM = c(1, 2), VISIT = c("BASELINE", "WEEK 1"))
SV <- data.frame(STUDYID = "S1", USUBJID = "01", VISITNUM = c(1, 2), VISIT = c("BASELINE", "WEEK 1"))
```

### Expected behavior

`generate_adam_code()` stops with a validation error. For example:

> Filter for ADLB depends on `SV.VISIT`, but `VISIT` is also a column of ADLB.
> Use `VISIT` without the domain prefix to filter on the ADLB column.
> mighty cannot filter on an external column that has the same name as a column of ADLB.

This fits the documented rule for global filters (`vignettes/adam_specification.qmd`): use a domain prefix in `depends` only for columns from outside the current domain, and write the expression without the prefix. A dependency without a prefix must be in `columns` (otherwise mighty gives "Missing columns: ADLB.<COL>"). With `depends: VISIT`, the example spec works and uses the `VISIT` copied from LB.

Proposed rule: a prefixed dependency in `global.depends` must not have the same name as a column in `columns`, except for the join keys.

### Actual behavior

`generate_adam_code()` succeeds. The generated program fails at run time:

```
Error in `dplyr::filter()`:
ℹ In argument: `VISIT == "BASELINE"`.
Caused by error:
! object 'VISIT' not found
```

### Environment

- R version: 4.6.1
- mighty version: 0.0.0.9044 (branch `fix/mightystandards-into-main`, commit 7f50559)
- mighty.standards: branch `dev/internal-components` (commit e690c8a). The internal components (`mighty_read_data`, `mighty_init_domain`, ...) are not on `main` yet, so I ran with `repos: ["local::<path>/mighty.standards/components"]`.
- Operating system: macOS

### Additional context

- Cause: `params_domain_filter_code()` joins the filter columns without a check for columns that are already in the domain.
- Fix location: `create_filter_domain_action()` (`R/add_filter_domain_actions.R`), with `throw_validation_error()`.
- Check against all of `columns`, not only the columns that exist when the filter runs. Otherwise `SV.VISIT` is still allowed when ADLB derives `VISIT` in a later step, and the spec is still unclear.
- Do not fix this by dropping the external column from the join. The filter would then use the wrong column with no message.
- Related: `depends: LB.VISIT` (base domain prefix) also fails, with a different error: `Missing columns: ADLB.` (empty column name). This can be a separate issue.

### Documentation

`vignettes/adam_specification.qmd` says: "Include domain prefix (e.g., `ADSL.SAFFL`) when referencing a column outside the current domain". It does not define "current domain" (base domain or ADaM domain?), and it does not say that a column in `columns` counts as inside. A user who follows it can write `SV.VISIT` or `LB.VISIT`.

Suggested change: define "current domain" as the ADaM domain, and add: "A column name without a prefix always refers to the ADaM domain. Do not use a domain prefix in `depends` for a column that is also in `columns`." This wording assumes the check covers all of `columns`.

### Impact

The user sees an R error from the generated program, not a spec validation error. The error message does not say which part of the spec is wrong.

### Related issues

- NovoNordisk-OpenSource/mighty.standards#69 ([review comment](https://github.com/NovoNordisk-OpenSource/mighty.standards/pull/69#discussion_r4389757847)). That PR adds a test that records the current component behavior.
