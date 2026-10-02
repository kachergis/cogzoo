# cogzoo

A community-contributed collection of computational models of cognition —
memory, categorization, learning, decision-making, and beyond — sharing a
common fitting/simulation interface, with bundled reference datasets and
reproducibility tests for every model.

## Why

Existing collections of cognitive models tend to be one-off scripts
scattered across a researcher's homepage or a defunct institutional
repository — the [Ohio State cognitive modeling
repository](http://www.cmr.osu.edu/) that partly motivated this package is
one such example, as is the loose collection of per-model folders in
[crsh/cognitive_models](https://github.com/crsh/cognitive_models) that most
of `cogzoo`'s initial models were migrated from. `cogzoo` aims to keep
implementations, the data they're fit to, and tests that they still work,
in one versioned, citable place, with a consistent interface so comparing
models doesn't mean learning a new calling convention for each one.

## Installation

```r
# install.packages("remotes")
remotes::install_github("kachergis/cogzoo")
```

## The interface

Every model is a `cogmodel` object implementing some of:

- `cm_simulate(model, ...)` — generate synthetic data
- `cm_loglik(model, data, ...)` — log-likelihood of data under the model
- `cm_fit(model, data, ...)` — fit by maximum likelihood (default: `optim()`
  over `model$params`; override for a custom fitting procedure, e.g. a
  non-likelihood objective or a Stan/JAGS backend)
- `cm_predict(model, ...)` — generate predictions

`cm_predict()`/`cm_fit()` only fix `model`/`data` in their signature —
different models need very different auxiliary inputs (a memory set, a
matrix of trial events, ...), so each model's method declares whatever
named arguments it actually needs. Check the model's own help page (e.g.
`?gcm`, `?minerva2`) for its interface.

See `vignette("getting-started")` for a full example fitting the
Generalized Context Model to Nosofsky (1989) data.

## Models

| Model | Domain | `cm_fit`? | Notes |
|---|---|---|---|
| `gcm()` | categorization | default (MLE) | Exemplar similarity → category choice probability. |
| `prototype()` | categorization | default (MLE) | Same similarity/choice rule as `gcm()`, but compares to one stored prototype per category instead of every exemplar. |
| `ebrw()` | decision-making | custom (weighted SSE) | GCM + random-walk choice/RT; fit criterion isn't a likelihood, so it overrides `cm_fit`. |
| `ebddm()` | decision-making | default (MLE, needs Suggested `RWiener`) | GCM-driven drift for a Wiener diffusion process. |
| `ddm()` | decision-making | default (MLE, needs Suggested `RWiener`) | General diffusion model with a free drift rate (not tied to a memory set). |
| `lba()` | decision-making | default (MLE, needs Suggested `rtdists`) | Race-to-threshold choice/RT model with a closed-form likelihood. |
| `minerva2()` | memory | none | Multiple-trace echo intensity/content; evaluated by curve shape, not per-trial MLE, in its original use. |
| `minerva_al()` | learning | default (MLE) | Instance-based cue-outcome learning. |
| `rescorla_wagner()` | learning | default (MLE) | Error-correction associative learning (blocking, overshadowing); deterministic given a trial sequence. |
| `q_learning()` | learning | default (MLE) | Delta-rule value learning with softmax choice, for repeated-choice/bandit tasks. |
| `cr_mpt()` | memory | default (MLE) | Conjoint recognition verbatim/gist/guessing tree, pooled aggregate-count form. |
| `sdt()` | memory | default (MLE) | Equal-variance Gaussian signal detection: the baseline for any old/new recognition judgment. |

Run `list_models()` for the live registry (domain, task types, citation).
More models are actively being migrated — see open issues/PRs for what's in
progress.

## Datasets

Bundled under `inst/extdata/<id>/`, each `.csv` paired with a `.yaml`
metadata sidecar (citation, license, task type, column dictionary) checked
by `validate_cogdata()`.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) — `add_model()` and `add_dataset()`
scaffold new contributions from templates, and both models and datasets are
expected to carry tests (simulate-and-recover, or an equivalent sanity
check, for models; schema validation for data).

## Status

Early but functional: the interface, registry, data-schema validator, and
twelve models across categorization, decision-making, memory, and learning
are in place and tested (`devtools::check()` is clean). Actively growing —
see [`CONTRIBUTING.md`](CONTRIBUTING.md) to add a model or dataset.
