parse\_spreadsheets
================
bryan
02 April 2019

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

    ## [1] "sample_run_2019-03-01_CORRECTED.xlsx"
    ## [2] "sample_run_2019-03-06_CORRECTED.xlsx"
    ## [3] "sample_run_2019-03-13_CORRECTED.xlsx"
    ## [4] "sample_run_2019-03-14_CORRECTED.xlsx"

data steps
----------

-   write two functions to process the data
-   function "nut\_data" reads a spreadsheet with the nutrient data
    -   selects and renames the columns
    -   filter out the calibration readings (type == "unknown")
    -   parse name into "sample\_date" and "sample\_site"
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
#' function to organize autoanalyzer nutrient data and correct concentrations for blanks
#' 
#' @param xlsx path to an untidy excel spreadsheet with the autoanalyzer data
nut_data <- function(xlsx) {

#read data & header info
f <- read_excel(xlsx, skip = 5)
h <- read_excel(xlsx, n_max = 3, col_names = FALSE)

# rename columns
names(f) <- tolower(names(f)) 
f <- rename(f, peak = "peak#", 
                n_raw_ht = "raw ht", n_cor_ht = "cor ht", n_um = um,
                p_raw_ht = "raw ht__1", p_cor_ht = "cor ht__1", p_um = um__1)

# filter type == "unknown"
f <- filter(f, type == "Unknown")

# parse name
y <- as.data.frame(str_split_fixed(f$name, " ", 2))
f$sample_date <- y[ , 1]
f$sample_site <- y[ , 2]

# create new id to parse identifier duplicates
f$id <-unlist(strsplit(f$identifier, "dup"))

# add header info to file
h <- read_excel(path = paste0(here::here('data/raw'), "/", files[i]), n_max = 3, col_names = FALSE)
f$run_name <- unlist(strsplit(as.character(h[1, 1]), ": "))[2]
f$run_date <- unlist(strsplit(as.character(h[2, 1]), ": "))[2]
f$operator <- unlist(strsplit(as.character(h[3, 1]), ": "))[2]

# select and reorder fields
f <- select(f, name, sample_date, sample_site, peak, position, identifier, id,
            n_raw_ht, n_cor_ht, n_um, p_raw_ht, p_cor_ht, p_um,
            run_name, run_date, operator)

# use find_outs to remove outliers and get means for blanks by id
bn <- find_outs(f, 'n')
bp <- find_outs(f, 'p')

# add blanks data to samples
s <- filter(f, sample_site != "milliQ") %>%
      left_join(bn) %>%
      left_join(bp)

# correct n_um and p_um
out <- mutate(s, n_um_cor = n_um - n_mean_b_um, p_um_cor = p_um - p_mean_b_um)

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
b <- filter(df, sample_site == "milliQ") %>%
        select(id1 = identifier, b_um = paste0(nutrient, "_um"))

# use boxplot to find outliers.  outliers are values outside 1.5 * the IQR
outliers <- boxplot(b$b_um, plot = FALSE)$out

# change outliers to NA
witch <- which(b$b_um %in% outliers)
b$b_um[witch] <- NA


# get means of consecutive blanks
b1 <- data.frame(id1 = as.numeric(c(1, b$id1)), id2 = as.numeric(c(b$id1, nrow(df))),
                 b_um1 = c(NA, b$b_um), b_um2 = c(b$b_um, NA)) 

b1 <- mutate(b1, mean_b_um = rowMeans(select(b1, b_um1, b_um2), na.rm = TRUE))

# loop through rows and create output
out <- c()
for(i in 1:nrow(b1)){
if(b1$id2[i]-b1$id1[i] > 1) {
a <- data.frame(id = seq((b1$id1[i] + 1), (b1$id2[i])), 
                b_um1 = b1$b_um1[i], b_um2 = b1$b_um2[i], mean_b_um = b1$mean_b_um[i])
out <- rbind(out, a)
}}

# add nutrient to names and ouput
out <- mutate(out, id = as.character(id))
names(out)[-1] <- paste0(nutrient, '_', names(out)[-1])
return(out)
}
```

``` r
# process all files
for(i in 1:length(files)){
x <- nut_data(paste0(here::here('data/raw'), "/", files[i]))
write.csv(x, paste0(here::here('output'), '/', x$run_name[1], '.csv'), row.names = FALSE)
}
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"

data definitions for added or modified fields
---------------------------------------------

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
<td>sample_date</td>
<td>parsed from the name field. If this is a sample this the first of the name field</td>
</tr>
<tr class="even">
<td>sample_site</td>
<td>parsed from the name field. If this is a sample this the second part of the name field</td>
</tr>
<tr class="odd">
<td>id</td>
<td>parsed from the &quot;identifier&quot; field. Some identifiers have a number + &quot;dup&quot;; &quot;dup&quot; is removed</td>
</tr>
<tr class="even">
<td>run_name</td>
<td>added from the header</td>
</tr>
<tr class="odd">
<td>run_date</td>
<td>added from the header</td>
</tr>
<tr class="even">
<td>operator</td>
<td>added from the header</td>
</tr>
<tr class="odd">
<td>p_b_um1</td>
<td>value of first phosphorus blank in a group</td>
</tr>
<tr class="even">
<td>p_b_um2</td>
<td>value of second phosphorus blank in a group</td>
</tr>
<tr class="odd">
<td>p_mean_b_um</td>
<td>mean of p_b_um1 &amp; p_b_um2 (or value if one is missing)</td>
</tr>
<tr class="even">
<td>p_um_cor</td>
<td>corrected phosphorus concentration (p_um - p_mean_b_um)</td>
</tr>
<tr class="odd">
<td>n_b_um1</td>
<td>value of first nitrogen blank in a group</td>
</tr>
<tr class="even">
<td>n_b_um2</td>
<td>value of second nitrogen blank in a group</td>
</tr>
<tr class="odd">
<td>n_mean_b_um</td>
<td>mean of n_b_um1 &amp; n_b_um2 (or value if one is missing)</td>
</tr>
<tr class="even">
<td>n_um_cor</td>
<td>corrected nitrogen concentration (n_um - n_mean_b_um)</td>
</tr>
</tbody>
</table>
