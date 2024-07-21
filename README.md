
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdtmsyn

<!-- badges: start -->
<!-- badges: end -->

sdtmsyn is an R package to generate synthetic SDTM data for fictional
clinical PK studies with the drug ‘examplinib’.

The generated data are based on simple heuristics for vital signs and a
popPK model for the drug and its metabolite and reflect a realistic
participant population.

## Installation

You can install the development version of sdtmsyn like so:

``` r
devtools::install_github("rstrotmann/sdtmsyn)
```

## Example

This is a basic example to generate SDTM domains DM, VS, EX and PC for a
fictional food effect study with ‘examplinib’:

``` r
library(sdtmsyn)

sdtm <- make_study_fe()
names(sdtm)
#> [1] "dm" "vs" "lb" "ex" "pc"
head(sdtm$dm, 5)
#>   SITEID  SUBJID         ACTARM ACTARMCD          RFICDTC          RFSTDTC
#> 1    101 1010001 Screen Failure SCRNFAIL 2000-12-23T11:20             <NA>
#> 2    101 1010002 Screen Failure SCRNFAIL 2000-12-23T10:01             <NA>
#> 3    101 1010003                      BA 2000-12-24T11:09 2001-01-05T11:09
#> 4    101 1010004                      AB 2000-12-25T11:25 2001-01-05T11:25
#> 5    101 1010005                      BA 2000-12-28T08:17 2001-01-06T08:17
#>           RFXSTDTC   STUDYID          USUBJID SEX AGE  AGEU COUNTRY DOMAIN
#> 1             <NA> 202400002 2024000021010001   M  32 YEARS     DEU     DM
#> 2             <NA> 202400002 2024000021010002   M  39 YEARS     DEU     DM
#> 3 2001-01-05T11:09 202400002 2024000021010003   F  40 YEARS     DEU     DM
#> 4 2001-01-05T11:25 202400002 2024000021010004   M  41 YEARS     DEU     DM
#> 5 2001-01-06T08:17 202400002 2024000021010005   F  45 YEARS     DEU     DM
#>                                          ARM    ARMCD                      RACE
#> 1                             Screen Failure SCRNFAIL                     WHITE
#> 2                             Screen Failure SCRNFAIL                     WHITE
#> 3 Fasted administration - Fed administration       BA                     WHITE
#> 4 Fed administration - Fasted administration       AB BLACK OR AFRICAN AMERICAN
#> 5 Fasted administration - Fed administration       BA                     WHITE
#>   ETHNIC          RFENDTC                                    ARCTARM
#> 1                    <NA>                             Screen Failure
#> 2                    <NA>                             Screen Failure
#> 3        2001-01-12T11:09 Fasted administration - Fed administration
#> 4        2001-01-12T11:25 Fed administration - Fasted administration
#> 5        2001-01-13T08:17 Fasted administration - Fed administration
head(sdtm$vs, 5)
#>     STUDYID          USUBJID VSTESTCD   VSORRES VSTEST VSORRESU VSSTRESN
#> 1 202400002 2024000021010001   HEIGHT 174.87563 Height       cm    174.9
#> 2 202400002 2024000021010001   WEIGHT  78.02786 Weight       kg     78.0
#> 3 202400002 2024000021010002   HEIGHT 187.64142 Height       cm    187.6
#> 4 202400002 2024000021010002   WEIGHT  67.98458 Weight       kg     68.0
#> 5 202400002 2024000021010003   HEIGHT 175.61113 Height       cm    175.6
#>   VSSTRESU     EPOCH DOMAIN VSBLFL     VISIT            VSDTC
#> 1       cm SCREENING     VS      Y SCREENING             <NA>
#> 2       kg SCREENING     VS      Y SCREENING             <NA>
#> 3       cm SCREENING     VS      Y SCREENING             <NA>
#> 4       kg SCREENING     VS      Y SCREENING             <NA>
#> 5       cm SCREENING     VS      Y SCREENING 2001-01-05T11:09
head(sdtm$ex, 5)
#> # A tibble: 5 × 13
#>   STUDYID   USUBJID     RFSTDTC DOMAIN EXTRT  EXDY EXDOSE EXROUTE EXDOSFRM EPOCH
#>   <chr>     <chr>       <chr>   <chr>  <chr> <dbl>  <dbl> <chr>   <chr>    <chr>
#> 1 202400002 2024000021… 2001-0… EX     RS20…     1    100 ORAL    TABLET   RAND…
#> 2 202400002 2024000021… 2001-0… EX     RS20…     8    100 ORAL    TABLET   RAND…
#> 3 202400002 2024000021… 2001-0… EX     RS20…     1    100 ORAL    TABLET   RAND…
#> 4 202400002 2024000021… 2001-0… EX     RS20…     8    100 ORAL    TABLET   RAND…
#> 5 202400002 2024000021… 2001-0… EX     RS20…     1    100 ORAL    TABLET   RAND…
#> # ℹ 3 more variables: EXSTDTC <chr>, EXENDTC <chr>, EXSEQ <int>
head(sdtm$pc, 5)
#> # A tibble: 5 × 13
#>   USUBJID    NTIME PCTESTCD PCSTRESN PCTEST STUDYID DOMAIN PCELTM PCTPTNUM PCDTC
#>   <chr>      <dbl> <chr>       <dbl> <chr>  <chr>   <chr>  <chr>     <dbl> <chr>
#> 1 202400002…   0   RS2023       0    RS2023 202400… PC     PT0H        0   2001…
#> 2 202400002…   0   RS20234…     0    RS202… 202400… PC     PT0H        0   2001…
#> 3 202400002…   0.5 RS2023      31.8  RS2023 202400… PC     PT0.5H      0.5 2001…
#> 4 202400002…   0.5 RS20234…     7.18 RS202… 202400… PC     PT0.5H      0.5 2001…
#> 5 202400002…   1   RS2023     134.   RS2023 202400… PC     PT1H        1   2001…
#> # ℹ 3 more variables: PCSEQ <int>, PCSPEC <chr>, PCRFTDTC <chr>
```
