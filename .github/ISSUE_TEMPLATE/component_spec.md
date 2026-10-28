---
name: Component specification
about: Specify a new standard component (parameters, contract, test cases)
title: "[FEAT] Component name"
labels: ''
assignees: ''

---

## Description

[One sentence: what the component derives. For example: "Calculate Analysis Day (ADY) relative to TRTSDT."]

## Parameters

[List the values that are parameters in the mustache template. Each parameter must be set in the ADaM specification that uses the component. Say which values are hardcoded in the template and are not parameters.]

[Example table. Replace the rows with the parameters of your component.]

| Parameter | Type | Meaning |
|---|---|---|
| `domain` | character | Name of the ADaM dataset |
| `...` | ... | ... |

## Contract

Number the clauses in one sequence across all three sections. Test cases refer to these numbers.

**Derivation**

[What the output is, and the rule for each case. Include edge cases: boundaries, missing values, datetimes.]

1. ...
2. ...

**Requires**

[Conditions the input must meet. The component does not check these.]

3. ...

**Guarantees**

[Properties of the output that are true for every input that meets the requirements. For example, type and allowed values.]

4. ...

Basis in the standard: [Give the document, version and section for each rule (for example ADaMIG v1.3 §3.1.2 item 8). Say which points the standard does not address.]

## Test cases

[State shared inputs, for example a default anchor date or time zone.]

| Test | Description | Input data | Expected result | Clauses |
|---|---|---|---|---|
| 1 | ... | ... | ... | ... |

[Say which Guarantees clauses every test also checks.]

## mighty framework rules the tests must also check in addition to component-specific tests

- The component reads only [list of variables].
- The component changes only [list of output variables].
- No warnings and no messages for inputs that meet the requirements.
- `@type [type]`: [for `column`: rows and their order do not change. For `row` and `parameter`: no columns are added or removed].

## Requires clauses that need a data-quality check and can't be checked by component unit-tests

[Copy the Requires clauses, with their numbers, that unit tests cannot check.]
