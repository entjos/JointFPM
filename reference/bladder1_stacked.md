# Stacked version of the bladder1 dataset included in the survival package

This dataset includes the bladder1 dataset included the survival
package, which has been transformed into stacked format for use with
`JointFPM`. The stacked datset includes one row per individual for the
competing event and one rows per individual for each reoccurrence of
bladder cancer.

## Usage

``` r
bladder1_stacked
data(bladder1_stacked)
```

## Format

A data frame with 412 rows and 11 columns

## Details

For more information please take a look at
[`?survival::bladder`](https://rdrr.io/pkg/survival/man/bladder.html).
