## Resubmission

This is a resubmission. The automatic pre-tests reported an overall check time
of 12 min on Windows. In this version I have:

* Loaded the two hidden Markov models of the vignette from a small file
  fitted beforehand with the code shown (data-raw/vignette_hmm.R); the rest of
  the vignette still runs during the build.
* Called 'ctmm' and 'momentuHMM' only through `::`, so they are loaded when
  needed and not every time the package is loaded.

The check time on win-builder (R-devel) is now 179 s.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Possibly misspelled words in DESCRIPTION: 'ARP' is the abbreviation of
  "area-restricted phase", defined in the same sentence, and 'unanchored'
  is a technical term for movements made before the first area-restricted
  phase. Both are spelled correctly.

## Test environments

* local macOS (aarch64), R 4.5.0
* win-builder, R-devel
* win-builder, R-release
