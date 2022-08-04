# (PART\*) PROJECT 1 {-}

Objective: We want to simplify a network by identifying nodes sharing similar characteristics.

Solution: We can classify the nodes into subgroups. Subgroups are clusters of nodes with similar characteristics (e.g. they belong to the same taxonomy, or they eat similar food). However, we do not know beforehand what the subgroups will be.

# Data preprocessing
We will use the food web network compiled by Planque et al. (2014) which is available from the repository ['Ecological Archives'](https://esapubs.org/archive/search.php?journal=E&year=2014&firstAuthor=Planque&isDP=1) (ID E095-124).

_N.B._ If needed, the dataset can be retrieved from:

- the Journal "Ecology",
- published in Year "2014",
- by the First author "Planque",
- as a Data Paper ("Yes").

## Load dependencies

```r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
#> ✔ tibble  3.1.8     ✔ dplyr   1.0.9
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.0
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> ── Conflicts ──────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(magrittr)
#> 
#> Attaching package: 'magrittr'
#> 
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> 
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
```

## Data importation



The dataset is provided as tables formatted inside text files.

### Import species

```r
species_list <- read.delim("raw_data/Barents_Sea/SpeciesList_2015.txt", header = TRUE)
head(species_list)
#>           TROPHOSPECIES ABBREVIATION         PHYLUM_SUBPYLUM                   CLASS
#> 1              DETRITUS      DET_IND                Detritus                Detritus
#> 2  AUTOTHROPH_FLAGELLAT      AUT_FLA   Autotroph flagellates   Autotroph flagellates
#> 3        BACTERIA_INDET      BAC_IND            Picoplankton            Picoplankton
#> 4                DIATOM       DIATOM           Microplankton           Microplankton
#> 5 HETEROTROPH_FLAGELLAT      HET_FLA Heterotroph flagellates Heterotroph flagellates
#> 6             ICE_ALGAE      ICE_ALG               Ice algae               Ice algae
#>                     ORDER                  FAMILY      GROUP
#> 1                Detritus                Detritus 1_Plankton
#> 2   Autotroph flagellates   Autotroph flagellates 1_Plankton
#> 3            Picoplankton            Picoplankton 1_Plankton
#> 4           Microplankton           Microplankton 1_Plankton
#> 5 Heterotroph flagellates Heterotroph flagellates 1_Plankton
#> 6               Ice algae               Ice algae 1_Plankton
```

The list of species is provide as a table that contains the species names along with an abbreviation and the taxonomy (i.e. classification of each organism).

### Import interactions

```r
pairwise_list <- read.delim("raw_data/Barents_Sea/PairwiseList_2015.txt", header = TRUE)
head(pairwise_list)
#>             PWKEY    PREDATOR                  PREY CODE
#> 1 ACA_SPP-ACA_SPP ACARTIA_SPP           ACARTIA_SPP    2
#> 2 ACA_SPP-AUT_FLA ACARTIA_SPP  AUTOTHROPH_FLAGELLAT    1
#> 3  ACA_SPP-DIATOM ACARTIA_SPP                DIATOM    1
#> 4 ACA_SPP-HET_FLA ACARTIA_SPP HETEROTROPH_FLAGELLAT    1
#> 5 ACA_SPP-MIX_FLA ACARTIA_SPP MIXOTROPH_FLAGELLATES    4
#> 6  ACA_SPP-PROZOO ACARTIA_SPP      PROTOZOOPLANKTON    1
```
The list of trophic interactions (a.k.a. relationships of 'who eats and whom') is provided as a pairwise list. The first column contains an identifier.
The consecutive collumns PREDATOR and PREY contains the  of the predator and prey, respectively. The row contains the relationship between a predator and a prey.

### Import literature references

```r
references <- read.delim("raw_data/Barents_Sea/References_2015.txt")
pairwise_to_references <- read.delim("raw_data/Barents_Sea/Pairwise2References_2015.txt")
```
Both 'references' and 'pairwise_to_references' tables contain metadata about the interactions.

## Clean datat set

Correct mispelling in column names:

```r

species_list %<>% dplyr::rename(., PHYLUM_SUBPHYLUM = PHYLUM_SUBPYLUM)
```

Correct mispelling in species name

1. Identify patterns using regular expressions while excluding the "_" in the names
2. Check what patterns need replacing
2. Replace the patterns

```r

# Helper function to apply to each column

# 
# has_non_letters <- apply(species_list, 2, FUN = find_patterns, to_find = "[^\\p{L} ^_]")
# 
# 
# try <- species_list %>% mutate(
#   has_non_letters = grepl("[^\\p{L} ^_]", df$names, perl = TRUE)
# )

```
### Split columns
We can extract more information from the 'trophospecies' column that contains both the Genus and the species name. We can use that additional information for the classification.

_N.B._ It does not matter if you do not know what Genus and species names are. Just know that they are deeper levels in the taxonomy of an organism.




## 
