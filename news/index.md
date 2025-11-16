# Changelog

## JointFPM (development version)

## JointFPM 1.3.0

CRAN release: 2025-07-26

- Stable release of `mean_no` function

## JointFPM 1.2.2

CRAN release: 2025-02-20

- The DESCRIPTION file now includes a link to the article in the
  Biometrical Journal describing the method implemented in this package.
- The package now requieres R \>= 4.1.0, due to the use of the native
  pipe operator. This fixes a note in the CRAN checks

## JointFPM 1.2.1

CRAN release: 2024-06-19

- Added
  [`summary.JointFPM()`](https://entjos.github.io/JointFPM/reference/summary.JointFPM.md),
  which provides a nicer overview of the model estimates
- [`predict.JointFPM()`](https://entjos.github.io/JointFPM/reference/predict.JointFPM.md)
  includes new `control` and `...` arguments, which are passed to
  `rstpm2::gms()` and can be used to control the estimation procedure
  (pull request [\#12](https://github.com/entjos/JointFPM/issues/12) by
  [@ellessenne](https://github.com/ellessenne)).

## JointFPM 1.2.0

CRAN release: 2024-01-22

- `predict.JointFP()` allows now to chose Gaussian quadrature instead of
  Romberg’s method for the integration of the production of the survival
  and intensity function for estimating the mean number of events
  ([@ellessenne](https://github.com/ellessenne),
  [\#8](https://github.com/entjos/JointFPM/issues/8)). Using Gaussian
  quadrature is fast while providing results similar to Romberg’s
  method, if a sufficient number if nodes is chosen. This might be
  particular useful when standardising over linear covariates.
- Fixed a small error when upgrading to Roxygen 7.3.0
  [\#10](https://github.com/entjos/JointFPM/issues/10).

## JointFPM 1.1.0

CRAN release: 2023-11-27

- Added standardization for estimating marginal estimates of the mean
  number of events and differences thereof
- Added checks of user inputs to
  [`JointFPM()`](https://entjos.github.io/JointFPM/reference/JointFPM.md)
- Added checks of user inputs to
  [`predict.JointFPM()`](https://entjos.github.io/JointFPM/reference/predict.JointFPM.md)
- Improved error messages
- Improved testing coverage

## JointFPM 1.0.1

CRAN release: 2023-10-13

- Fixed bug when calculating difference between two mean number of
  events functions
- Added tests for predict function

## JointFPM 1.0.0

CRAN release: 2023-09-23

- First stable release
