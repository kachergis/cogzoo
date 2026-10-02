# Hierarchical Bayesian conjoint recognition (reference JAGS models)

`aggregated_cr_model.jags` and `cr_two_within_conditions.jags` are the
original per-subject hierarchical Bayesian versions of the conjoint
recognition MPT (ported unchanged from this monorepo's `cr-mpt/` folder).
They estimate subject-level `V`/`G`/`b` with beta priors, optionally
crossed with two within-subject factors.

`cr_mpt()` (see `R/model-cr-mpt.R`) implements the pooled, aggregate-count
version of the same three-parameter tree as a proper `cogmodel` fit by
maximum likelihood — no JAGS required. These files are kept as reference
for anyone who wants to add a hierarchical variant, e.g. a
`cm_fit.cr_mpt_hierarchical()` method built on `rjags`/`R2jags` (which
would need the JAGS binary installed separately, so keep any such
dependency in `Suggests`, gated behind `requireNamespace()`, following the
pattern in `R/model-ebddm.R`).
