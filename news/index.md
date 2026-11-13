# Changelog

## disperseR 0.1.0

- Initial CRAN submission.
- Workflow to classify movement phases:
  [`divide.track.by.gap()`](https://cherraiz.github.io/disperseR/reference/divide.track.by.gap.md),
  [`resample.ctmm()`](https://cherraiz.github.io/disperseR/reference/resample.ctmm.md),
  [`fitHMM.wrap()`](https://cherraiz.github.io/disperseR/reference/fitHMM.wrap.md),
  [`get.epsilon()`](https://cherraiz.github.io/disperseR/reference/get.epsilon.md),
  [`get.arp()`](https://cherraiz.github.io/disperseR/reference/get.arp.md),
  [`link.arp()`](https://cherraiz.github.io/disperseR/reference/link.arp.md),
  [`class.mov()`](https://cherraiz.github.io/disperseR/reference/class.mov.md)
  and
  [`transfer.disperseR()`](https://cherraiz.github.io/disperseR/reference/transfer.disperseR.md).
- Plots:
  [`draw.step.densities()`](https://cherraiz.github.io/disperseR/reference/draw.step.densities.md),
  [`draw.linked.clusters()`](https://cherraiz.github.io/disperseR/reference/draw.linked.clusters.md),
  [`draw.track()`](https://cherraiz.github.io/disperseR/reference/draw.track.md)
  and
  [`draw.timeline()`](https://cherraiz.github.io/disperseR/reference/draw.timeline.md).
- [`phase.summary()`](https://cherraiz.github.io/disperseR/reference/phase.summary.md)
  builds a table with one row per movement phase, and
  [`print()`](https://rdrr.io/r/base/print.html) gives a short summary
  of `disperseR` objects.
- Simulated example data: `sim_tracks` and `sim_states`.
- Vignette
  [`vignette("disperseR")`](https://cherraiz.github.io/disperseR/articles/disperseR.md):
  full workflow and sensitivity of the classification to the main
  arguments.
