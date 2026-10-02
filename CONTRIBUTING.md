# Contributing to cogzoo

## Adding a model

1. `devtools::load_all(); add_model("yourmodel", domain = "memory")` —
   generates `R/model-yourmodel.R` and `tests/testthat/test-yourmodel.R` from
   the templates in `inst/templates/`.
2. Implement `cm_predict.yourmodel()` and `cm_loglik.yourmodel()`. Only add a
   `cm_fit.yourmodel()` method if the model needs something other than
   `optim()` over `model$params` (e.g. a Stan/JAGS backend) — otherwise the
   inherited `cm_fit.default` just works.
3. Fill in the `register_model()` call at the bottom of the file: domain,
   task type(s), a one-sentence description, and the citation for the
   original model.
4. Flesh out the generated test file. At minimum:
   - predictions are well-formed (e.g. probabilities in `[0, 1]`);
   - **simulate-and-recover**: simulate data from a model with known
     parameters via `cm_simulate()`, fit a model started from different
     values via `cm_fit()`, and check the recovered parameters land close to
     the truth. This is the standard bar for a new model — it's what
     actually verifies the log-likelihood is correct and the parameters are
     identifiable, not just that the code runs.
5. If you're reproducing a published model-fitting result, add a short
   vignette under `vignettes/` fitting the model to a bundled dataset and
   checking against a reported statistic (fitted parameter, log-likelihood,
   R²) from the source paper.
6. `devtools::document(); devtools::check()` before opening a PR.

## Adding a dataset

1. `add_dataset("author_year_label", data_path = "path/to/your.csv")` —
   creates `inst/extdata/author_year_label/` with your CSV and a metadata
   sidecar template.
2. Fill in every field of the `.yaml` sidecar, especially `license`:
   - if you collected or own the data, pick an open license (CC-BY-4.0 is a
     reasonable default);
   - if it's someone else's published data, state clearly what
     redistribution rights you actually have — "reproduced from Table 2 of
     \<citation\>" is not the same as "the author granted redistribution
     rights," and readers need to know which applies. When in doubt, ask the
     original author before adding data to this repo, and say so in the
     sidecar once you have.
3. `validate_cogdata("inst/extdata/author_year_label/author_year_label.csv")`
   should pass. It also runs automatically in CI over every bundled dataset.

## Why the tests matter here

Two kinds of correctness are checked, and they fail for different reasons:

- **Data validation** (`validate_cogdata()`) catches malformed contributions
  — a column typo'd between the CSV and its sidecar, a missing citation.
- **Model tests** (simulate-and-recover, prediction-range checks) catch
  implementation bugs that produce a *plausible-looking but wrong* model —
  the kind of bug a casual read of the code won't surface. A model without a
  recovery test is not considered reviewable.
