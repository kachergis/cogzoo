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
one such example (the site is no longer maintained and several of its
dataset/model links are dead), as is the loose collection of per-model
folders in [crsh/cognitive_models](https://github.com/crsh/cognitive_models)
that most of `cogzoo`'s initial models were migrated from. `cogzoo` aims to
keep implementations, the data they're fit to, and tests that they still
work, in one versioned, citable place, with a consistent interface so
comparing models doesn't mean learning a new calling convention for each
one.

## Related work

- **[catlearn](https://catlearn.r-forge.r-project.org/)** (Wills, Dome,
  Edmunds, et al.) is the closest match in both spirit and current health:
  an actively maintained CRAN package (v1.1, 2025-03-31; GitHub pushed as
  recently as September 2026) built on the explicit "Open Models" philosophy
  — formal models plus independently-replicated benchmark datasets plus a
  simulation archive — that `cogzoo` is also pursuing. It covers
  categorization and learning specifically: ALCOVE, COVIS, DIVA, EXIT,
  Gluck & Bower (1988), Mackintosh (1975), and SUSTAIN, among others —
  several of which were on `cogzoo`'s own "harder ones, later" list. Its
  interface is a flat set of exported functions per model (`slpALCOVE()`,
  `slpSUSTAIN()`, etc., "slp" = sequential learning process) rather than a
  shared object/generic interface, and it leans on `Rcpp`/`RcppArmadillo`
  for performance. Practical implication: `cogzoo` should wrap `catlearn`'s
  validated implementations as the backend for its own `alcove()`/
  `sustain()` (the way `ddm()`/`lba()` already wrap `RWiener`/`rtdists`)
  rather than reimplementing them from scratch.
- **[cognitivemodels](https://github.com/JanaJarecki/cognitivemodels)**
  (Jarecki & Seitz) is the closest prior art in spirit — an `lm()`-style
  formula interface over a common cognitive-model superclass, with a
  published ICCM paper behind it. It's now effectively dormant (last commit
  Dec 2022, 37 open issues, never reached CRAN, and depends on a second
  GitHub-only package that's been untouched since 2020), has a much heavier
  dependency footprint (requires a C++ toolchain plus `ROI`/`Rsolnp`/
  `quadprog`/`arrangements`), and its domain emphasis is risk/preference
  economics (shortfall theory, foraging, cumulative prospect theory) rather
  than the memory/categorization/learning core `cogzoo` started from. The
  only direct model overlap is GCM. Not a dependency candidate, but worth
  knowing about.
- **[cogmod](https://cran.r-project.org/package=cogmod)** (Makowski) is
  distinct by design, not a competitor: it's a `brms` extension providing
  custom response-distribution families (DDM, LBA, racing diffusion,
  lognormal race, ex-Gaussian, ordered-beta rating models) for hierarchical
  *Bayesian regression* with `brms`'s formula syntax, rather than a
  standalone model object with its own `cm_fit()`. It's under very active
  development (CRAN release 2026-09-25) by a maintainer with a strong track
  record (the easystats ecosystem). `ddm()`/`lba()` and `cogmod` solve
  different problems — ad hoc parameter estimation vs. multilevel Bayesian
  fits with per-subject random effects — but it's a natural pointer for
  anyone who outgrows `cogzoo`'s default optimizer-based fitting for those
  two models, and a candidate source of battle-tested densities if
  `RWiener`/`rtdists` ever need replacing.

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
| `gcm_recognition()` | memory | default (MLE; needs a few restarts) | GCM for old/new recognition (summed similarity → familiarity); reproduces Shin & Nosofsky's (1992) published Exp. 1 estimates on the bundled data. |
| `sdt()` | memory | default (MLE) | Equal-variance Gaussian signal detection: the baseline for any old/new recognition judgment. |

Run `list_models()` for the live registry (domain, task types, citation).
More models are actively being migrated — see open issues/PRs for what's in
progress.

## Datasets

Bundled under `inst/extdata/<id>/`, each `.csv` paired with a `.yaml`
metadata sidecar (citation, license, task type, column dictionary) checked
by `validate_cogdata()`.

- `nosofsky_1989` — categorization responses (Nosofsky, 1989).
- `shin_nosofsky1992_stimuli` + `shin_nosofsky1992_responses` — 6-D MDS
  stimulus coordinates and old/new recognition response proportions for
  three dot-pattern categories (Shin & Nosofsky, 1992); the two tables join
  on `(category, item_index)`.
- `kurtz2013_shj`, `lewandowsky2011_shj` (+ its `..._workingmemory`
  companion), `nosofsky1994_shj`, `rehder2005_shj` — four independent
  trial/block-level replications of Shepard, Hovland, & Jenkins' (1961) six
  category-learning problem types, via
  [ajwills72/sixproblems](https://github.com/ajwills72/sixproblems). This
  is the standard benchmark for comparing categorization models on learning
  *difficulty* (the classic Type I < II < III,IV,V < VI ordering), so it
  pairs naturally with `gcm()`/`prototype()` now and with `alcove()`/
  `sustain()` once those land.

None of these datasets carry an explicit license from their original
source; each sidecar documents exactly how the data was obtained (who
requested it from the original authors, what published figure/statistic
was used to verify it) and says to check with the current maintainer before
further redistribution beyond this package.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) — `add_model()` and `add_dataset()`
scaffold new contributions from templates, and both models and datasets are
expected to carry tests (simulate-and-recover, or an equivalent sanity
check, for models; schema validation for data).

## Status

Early but functional: the interface, registry, data-schema validator, and
thirteen models across categorization, decision-making, memory, and learning
are in place and tested (`devtools::check()` is clean). Actively growing —
see [`CONTRIBUTING.md`](CONTRIBUTING.md) to add a model or dataset.
