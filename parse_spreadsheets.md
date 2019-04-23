parse\_spreadsheets
================
bryan
23 April 2019

Introduction
------------

-   Nutrient analysis is done on autoanalyzer and the results are returned as an untidy spreadsheet.
-   Need to read the spreadsheet
-   eliminate un-needed data (calibration stuff mostly)
    -   these are mostly !is.na(name)
-   correct readings by subtracting out the blanks
    -   analysis done in batches with each batch (usually) bracketed by 2 blanks at the beginning and two at the end
    -   the last batch only has blanks at the beginning.

data sources
------------

-   spreadsheets received from Adam Pimienta and stored in here::here('data') directory.
-   current data files:

<!-- -->

    ##  [1] "sample_run_2018-12-20_CORRECTED.xlsx" 
    ##  [2] "sample_run_2018-12-20b_CORRECTED.xlsx"
    ##  [3] "sample_run_2018-12-21_CORRECTED.xlsx" 
    ##  [4] "sample_run_2019-02-26b_CORRECTED.xlsx"
    ##  [5] "sample_run_2019-03-01_CORRECTED.xlsx" 
    ##  [6] "sample_run_2019-03-06_CORRECTED.xlsx" 
    ##  [7] "sample_run_2019-03-07_CORRECTED.xlsx" 
    ##  [8] "sample_run_2019-03-13_CORRECTED.xlsx" 
    ##  [9] "sample_run_2019-03-14_CORRECTED.xlsx" 
    ## [10] "sample_run_2019-03-19_CORRECTED.xlsx" 
    ## [11] "sample_run_2019-03-20_CORRECTED.xlsx" 
    ## [12] "sample_run_2019-03-26_CORRECTED.xlsx" 
    ## [13] "sample_run_2019-03-27_CORRECTED.xlsx"

data steps
----------

-   write two functions to process the data
-   function "nut\_data" reads a spreadsheet with the nutrient data
    -   selects and renames the columns
    -   filter out the calibration readings (type == "unknown")
    -   add header info to data file as columns "run\_name", "run\_date", "operator"
    -   process blanks (see below) and correct concentrations (um - blank\_um)
-   function "find\_outs": is a subroutine for function "nut\_data"
    -   reads formatted data.frame from "nut\_data" for a specified nutrient ("n" or "p")
    -   selects only the data for "blanks"
    -   tests for and removes outliers using boxplot (outliers are values outside 1.5 x the IQR)
    -   calculates the means of successive (if they exist) blanks
    -   returns a data frame with blanks and means(blanks) for each sample
-   process the 4 files received from Stephen Shivers
-   save results as .csv in folder "output"

``` r
# i <-1; xlsx <- paste0(here::here('data/raw'), "/", files[i])

#' function to organize autoanalyzer nutrient data and correct concentrations for blanks
#' 
#' @param xlsx path to an untidy excel spreadsheet with the autoanalyzer data
nut_data <- function(xlsx) {

#read data & header info
f <- read_excel(xlsx, skip = 5)
h <- read_excel(xlsx, n_max = 3, col_names = FALSE)

# select & rename columns
f <- f[1:11]
names(f) <- c("peak", "position", "identifier", "name", "type",
              "n_raw_ht", "n_cor_ht", "n_um",
              "p_raw_ht", "p_cor_ht", "p_um")

# filter type == "unknown" and change non-numeric values of n_um and p_um to NA
f <- filter(f, type == "Unknown") %>%
      mutate(n_um = as.numeric(n_um), p_um = as.numeric(p_um))

# add header info to file
f$run_name <- unlist(strsplit(as.character(h[1, 1]), ": "))[2]
f$run_date <- unlist(strsplit(as.character(h[2, 1]), ": "))[2]
f$operator <- unlist(strsplit(as.character(h[3, 1]), ": "))[2]

# use find_outs to remove outliers and get means for blanks by id
bn <- find_outs(f, 'n')
bp <- find_outs(f, 'p')

# add blanks data to samples
s <- slice(f, -grep("milliQ", f$name)) %>%
      left_join(bn) %>%
      left_join(bp)

# correct n_um and p_um & reorder fields
out <- mutate(s, n_um_cor = n_um - n_mean_b_um, p_um_cor = p_um - p_mean_b_um) %>%
        select(peak, name, position, identifier,
               n_raw_ht, n_cor_ht, n_um, n_um_cor, n_b_um1, n_b_um2, n_mean_b_um, 
               p_raw_ht, p_cor_ht, p_um, p_um_cor, p_b_um1, p_b_um2, p_mean_b_um,
               run_name, run_date, operator)

# select and reorder fields
f <- select(f, peak, name, position, identifier, 
            n_raw_ht, n_cor_ht, n_um, p_raw_ht, p_cor_ht, p_um,
            run_name, run_date, operator)


return(out)
}
```

``` r
#' function to find and remove outliers from blanks
#' 
#' @param df a dataframe of values from function ??????
#' @param nutrient this will be 'n' or 'p'
find_outs <- function(df, nutrient = 'n') {
# filter, select and rename fields
b <- slice(df, grep("milliQ", df$name)) %>%
        select(id1 = peak, b_um = paste0(nutrient, "_um"))

# use boxplot to find outliers.  outliers are values outside 1.5 * the IQR
outliers <- boxplot(b$b_um, plot = FALSE)$out

# change outliers to NA
witch <- which(b$b_um %in% outliers)
b$b_um[witch] <- NA


# get means of consecutive blanks
b1 <- data.frame(id1 = c(1, b$id1), id2 = c(b$id1, max(b$id1)),
                 b_um1 = c(NA, b$b_um), b_um2 = c(b$b_um, NA)) 

b1 <- mutate(b1, mean_b_um = rowMeans(select(b1, b_um1, b_um2), na.rm = TRUE))

# loop through rows and create output
out <- c()
for(i in 1:nrow(b1)){
if(b1$id2[i]-b1$id1[i] > 1) {
a <- data.frame(peak = seq((b1$id1[i] + 1), (b1$id2[i])), 
                b_um1 = b1$b_um1[i], b_um2 = b1$b_um2[i], mean_b_um = b1$mean_b_um[i])
out <- rbind(out, a)
}}

# add nutrient to names and ouput
names(out)[-1] <- paste0(nutrient, '_', names(out)[-1])
return(out)
}
```

``` r
# process all files
for(i in 1:length(files)){
x <- tryCatch(nut_data(paste0(here::here('data/raw'), "/", files[i])), 
              error = function(e) 
                print(paste0("Warning: ", files[i], " did not parse")))
if(is.data.frame(x)) write.csv(x, paste0(here::here('output'), '/', x$run_name[1], '.csv'), 
                               row.names = FALSE)
}
```

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Warning in rlang::eval_tidy(~as.numeric(p_um), <environment>): NAs
    ## introduced by coercion

    ## Joining, by = "peak"

    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 1 more problem

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

    ## New names:
    ## * `Raw Ht` -> `Raw Ht...6`
    ## * `Cor Ht` -> `Cor Ht...7`
    ## * uM -> uM...8
    ## * `Raw Ht` -> `Raw Ht...9`
    ## * `Cor Ht` -> `Cor Ht...10`
    ## * ... and 7 more problems

    ## New names:
    ## * `` -> ...1

    ## Joining, by = "peak"
    ## Joining, by = "peak"

data definitions
----------------

<table>
<colgroup>
<col width="11%" />
<col width="88%" />
</colgroup>
<thead>
<tr class="header">
<th>field</th>
<th>data definition</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>peak</td>
<td>sequential order of samples - some calibration samples removed so not complete</td>
</tr>
<tr class="even">
<td>name</td>
<td>the sample name often this is sample date and site</td>
</tr>
<tr class="odd">
<td>position</td>
<td>not sure what this is. Probably indicates position sample was loaded into analyzer</td>
</tr>
<tr class="even">
<td>identifier</td>
<td>appear to be a unique identifier for sample. Some are labelled &quot;dup&quot; for duplicate.</td>
</tr>
<tr class="odd">
<td>run_name</td>
<td>added from the header</td>
</tr>
<tr class="even">
<td>run_date</td>
<td>added from the header</td>
</tr>
<tr class="odd">
<td>operator</td>
<td>added from the header</td>
</tr>
<tr class="even">
<td>n_raw_ht</td>
<td>from the operator. Added &quot;n_&quot; to indicate this is a nitrogen sample</td>
</tr>
<tr class="odd">
<td>n_cor_ht</td>
<td>from the operator. Added &quot;n_&quot; to indicate this is a nitrogen sample</td>
</tr>
<tr class="even">
<td>n_um</td>
<td>from the operator. Added &quot;n_&quot; to indicate this is a nitrogen sample. This is the uncorrected nitrogen concentration.</td>
</tr>
<tr class="odd">
<td>n_b_um1</td>
<td>value of first nitrogen blank in a group of observations</td>
</tr>
<tr class="even">
<td>n_b_um2</td>
<td>value of second nitrogen blank in a group of observations</td>
</tr>
<tr class="odd">
<td>n_mean_b_um</td>
<td>mean of n_b_um1 &amp; n_b_um2 (or value if one is missing); NA means both were missing</td>
</tr>
<tr class="even">
<td>p_b_um1</td>
<td>value of first phosphorus blank in a group of observations</td>
</tr>
<tr class="odd">
<td>p_b_um2</td>
<td>value of second phosphorus blank in a group</td>
</tr>
<tr class="even">
<td>p_mean_b_um</td>
<td>mean of p_b_um1 &amp; p_b_um2 (or value if one is missing); NA means both were missing</td>
</tr>
<tr class="odd">
<td>p_um_cor</td>
<td>corrected phosphorus concentration (p_um - p_mean_b_um)</td>
</tr>
<tr class="even">
<td>n_b_um1</td>
<td>value of first nitrogen blank in a group of observations</td>
</tr>
<tr class="odd">
<td>n_b_um2</td>
<td>value of second nitrogen blank in a group of observations</td>
</tr>
<tr class="even">
<td>n_mean_b_um</td>
<td>mean of n_b_um1 &amp; n_b_um2 (or value if one is missing)</td>
</tr>
<tr class="odd">
<td>n_um_cor</td>
<td>corrected nitrogen concentration (n_um - n_mean_b_um); NOTE if NA this could mean that n_um &amp;/or n_mean_b_um is missing. Check manually.</td>
</tr>
<tr class="even">
<td>p_um_cor</td>
<td>corrected phosphorus concentration (p_um - p_mean_b_um); NOTE if NA this could mean that p_um &amp;/or p_mean_b_um is missing. Check manually.</td>
</tr>
</tbody>
</table>
