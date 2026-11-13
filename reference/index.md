# Package index

## Prepare the tracks

Split tracks at monitoring gaps and regularise them.

- [`divide.track.by.gap()`](https://cherraiz.github.io/disperseR/reference/divide.track.by.gap.md)
  : Split trajectory by time gap
- [`resample.ctmm()`](https://cherraiz.github.io/disperseR/reference/resample.ctmm.md)
  : Resample tracking data using Continuous-Time Movement Models (CTMM)

## Behavioural states

Fit hidden Markov models and inspect the step lengths of each state.

- [`fitHMM.wrap()`](https://cherraiz.github.io/disperseR/reference/fitHMM.wrap.md)
  : Fit Hidden Markov Models for Movement Data Classification
- [`draw.step.densities()`](https://cherraiz.github.io/disperseR/reference/draw.step.densities.md)
  : Plot Step Length Densities by Behavioral State

## Area-restricted phases

Choose the clustering distance, identify ARPs and link them through
time.

- [`get.epsilon()`](https://cherraiz.github.io/disperseR/reference/get.epsilon.md)
  : Calculate \\\epsilon\\ threshold for spatial clustering
- [`get.arp()`](https://cherraiz.github.io/disperseR/reference/get.arp.md)
  : Identify Area-Restricted Phase (ARP) clusters
- [`link.arp()`](https://cherraiz.github.io/disperseR/reference/link.arp.md)
  : Link Area-Restricted Phase (ARP) Clusters
- [`draw.linked.clusters()`](https://cherraiz.github.io/disperseR/reference/draw.linked.clusters.md)
  : Plot linked Area-Restricted Phase (ARP) clusters

## Movement phases

Classify the movements between ARPs and transfer them to the original
locations.

- [`class.mov()`](https://cherraiz.github.io/disperseR/reference/class.mov.md)
  : Classify movement phases
- [`transfer.disperseR()`](https://cherraiz.github.io/disperseR/reference/transfer.disperseR.md)
  : Transfer classification data from disperseR object to a base dataset

## Summaries and plots

- [`phase.summary()`](https://cherraiz.github.io/disperseR/reference/phase.summary.md)
  : Summarise movement phases
- [`draw.track()`](https://cherraiz.github.io/disperseR/reference/draw.track.md)
  : Plot movement tracks
- [`draw.timeline()`](https://cherraiz.github.io/disperseR/reference/draw.timeline.md)
  : Plot the timeline of movement phases
- [`print(`*`<disperseR>`*`)`](https://cherraiz.github.io/disperseR/reference/print.disperseR.md)
  : Print a disperseR object

## Example data

- [`sim_tracks`](https://cherraiz.github.io/disperseR/reference/sim_tracks.md)
  : Simulated dispersal tracks
- [`sim_states`](https://cherraiz.github.io/disperseR/reference/sim_states.md)
  : Simulated dispersal tracks with behavioural states
