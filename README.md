# US Census

Analysis of US census data on income.

This repo contains both the code and the (compressed) data required for the analysis.

The analysis was done in R using the `tidyverse`. `tidymodels` was used for the modeling.

The analysis consists of:

* [an exploratory part](Rmd/eda.md),
* and [a modeling part](Rmd/modeling.md).


In order to be able to run the Rmarkdown files locally, you first need to unzip the compressed data:

    ```
    unzip us_census_full.zip
    ```

Have fun!
