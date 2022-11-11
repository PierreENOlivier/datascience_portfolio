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
# rm(list = ls())
library(tidyverse)
library(magrittr)

old_opts <- options() # preserve current preferences for after we exist the function
options(box.path = c("./scripts", 
                       "../scripts",
                       "./scripts/box",
                       "../scripts/box") )
  
```

Load modules

```r
box::use(box/modify_strings[locate_pattern, replace_pattern])
# box::use(box/refresh_box[refresh])
# refresh("box/modify_strings")
         
```

## Data importation

The dataset is provided as tables formatted inside text files.

### Import species list

```r
species_list <- read.delim("raw_data/Barents_Sea/SpeciesList_2015.txt", header = TRUE)
sp_save <- species_list # keep original copy
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

### Import interactions list

```r
pairwise_list <- read.delim("raw_data/Barents_Sea/PairwiseList_2015.txt", header = TRUE)
pw_save <- species_list # keep original copy
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
The consecutive columns PREDATOR and PREY contains the species names of the predator and prey, respectively. The rows contain the relationships between a predator and a prey.

### Import literature references

```r
references <- read.delim("raw_data/Barents_Sea/References_2015.txt")
pairwise_to_references <- read.delim("raw_data/Barents_Sea/Pairwise2References_2015.txt")
```
Both 'references' and 'pairwise_to_references' tables contain metadata about the interactions.

## Data cleaning and augmentation
### Species list

#### Correct mispelling and non-letter characters

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
box::use(box/modify_strings[locate_pattern])
strings_to_correct <- locate_pattern(species_list, "[^0-9a-zA-Z_]")
#> NA rows contain the pattern: [^0-9a-zA-Z_] 
#> Column: 1
#> Column: 2
#> Column: 3
#> Column: 4
#> Column: 5
#> Column: 6
#> Column: 7
print(strings_to_correct)
#> [1] "BERO\xe8_SP"                    "OITHONA_SPINIROSTRIS/ATLANTICA"
#> [3] "Autotroph flagellates"          "Heterotroph flagellates"       
#> [5] "Ice algae"                      "Mixotroph flagellates"         
#> [7] "Amphipoda/Gammarida"            "Amphipoda/Gammaridea"          
#> [9] "Amphipoda/Hyperiidea"
```
The organisms names are inconsistent and contain [**ASCII strings**](https://www.ascii-code.com/), slashes, and spaces that R won't handle properly.
We will replace them.


```r
# Store the patterns
patterns <- c("\xe8", " ", "/")

# Store their replacements
replacements <- c("E", "_", "_")

# Replace the patterns in all columns
box::use(box/modify_strings[replace_pattern])

species_list <- replace_pattern(x = species_list, vector_pattern = patterns, vector_replacement = replacements )
#> Replacing '<e8>' with 'E' 
#> Replacing ' ' with '_' 
#> Replacing '/' with '_'
sp_save <- species_list
# species_list <- sp_save
```

Some of the latin name abbreviations are associated with the wrong organism. The abbreviations follow international fisheries standards (e.g. GAD_MOR for 'GADUS MORHUA'). We can reconstruct the abbreviations from the organisms' names.


```r

# "(^[A-Za-z]{1,3})"
# "_+([A-Za-z]{1,3})"

abbr1 <- species_list$TROPHOSPECIES %>% str_extract(., "(^[A-Za-z]{1,3})")
abbr2 <- species_list$TROPHOSPECIES %>% str_extract(., "(?<=_)([A-Za-z]{1,3})")
species_list$ABBREVIATION <- paste(abbr1, ifelse(is.na(abbr2), "IND", abbr2), sep="_" )
```


#### Data augmentation

1. Genus + Species

We can extract more information from the 'trophospecies' column that contains both the Genus and the species name. We can use the Genus as additional information for the classification.

_N.B._ It does not matter if you do not know what Genus and species names are. Just know that they are deeper levels in the taxonomy of an organism.

Genus and species are grouped with '_'. We can split them.


```r
species_list %<>% separate(., col = TROPHOSPECIES, c("GENUS", "SPECIES"), 
                           remove = FALSE, extra = "drop" )
#> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 6 rows [1, 4, 7, 10, 80,
#> 111].
```
Finally, we can drop the column species because all species are different and cannot be used to group information.


```r
# Drop SPECIEs
species_list %<>% select(-SPECIES)

# Reorganize the species list to follow the taxonomy
species_list %<>% relocate(GENUS, .after = FAMILY)
```

## List of interactions: pairwise list
### Data cleaning

For the purpose of the analysis, the data in 'species_list' and 'pairwise_list' need to match.
We need to apply the same cleaning to this table.

#### Correct mispellings

```r
# debug(locate_pattern)
box::use(box/modify_strings[locate_pattern])

box::use(box/modify_strings[replace_pattern])

strings_to_correct <- locate_pattern(pairwise_list, "[^0-9a-zA-Z_-]")
print(strings_to_correct)
pairwise_list <- replace_pattern(x = pairwise_list, vector_pattern = patterns, vector_replacement = replacements )

#### Correct PWKEY
"(^[A-Za-z]{1,3})"
"_+([A-Za-z]{1,3})"

# debug(extract_and_merge)
extract_and_merge(pairwise_list$PREDATOR, c("(^[A-Za-z]{1,3})", "_+([A-Za-z]{1,3})_?"), sep = ""   )

box::use(box/modify_strings[test_extract])
test_extract(pairwise_list$PREDATOR, "_+([A-Za-z]{1,3})(?(_)([A-Za-z]{1,3}))")

```

### Data transformation
The data was provided as a long pairwise list which is limiting for the purpose of the analysis. We can convert the list to a squared binary matrix where the intersections of rows and columns contain the information for an interaction.

In other words, '0s' means that the interaction is absent (or has yet to be observed in nature); '1s' indicates the presence of an interaction between the organisms listed in the columns and rows interacting.

## 
