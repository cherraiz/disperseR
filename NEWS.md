# disperseR 0.1.0

* Initial CRAN submission.
* Workflow to classify movement phases: `divide.track.by.gap()`,
  `resample.ctmm()`, `fitHMM.wrap()`, `get.epsilon()`, `get.arp()`,
  `link.arp()`, `class.mov()` and `transfer.disperseR()`.
* Plots: `draw.step.densities()`, `draw.linked.clusters()`, `draw.track()` and
  `draw.timeline()`.
* `phase.summary()` builds a table with one row per movement phase, and
  `print()` gives a short summary of `disperseR` objects.
* Simulated example data: `sim_tracks` and `sim_states`.
* Vignette `vignette("disperseR")`: full workflow and sensitivity of the
  classification to the main arguments.
