Soil Profile Statistical Analysis
================
Kar
2022-07

-   [1 R PACKAGES](#1-r-packages)
-   [2 DATA PREPARATION](#2-data-preparation)
    -   [2.1 Data Import](#21-data-import)
    -   [2.2 Data Description](#22-data-description)
    -   [2.3 Data Exploration](#23-data-exploration)
-   [3 DATA CLEANING](#3-data-cleaning)
    -   [3.1 Removing Group and Gp](#31-removing-group-and-gp)
    -   [3.2 Rearrange](#32-rearrange)
    -   [3.3 Trimming](#33-trimming)
    -   [3.4 Rearrange levels](#34-rearrange-levels)
-   [4 EXPLORATORY DATA ANALYSIS](#4-exploratory-data-analysis)
    -   [4.1 Univariate histogram](#41-univariate-histogram)
    -   [4.2 Soil Chemistries against Soil
        Depth](#42-soil-chemistries-against-soil-depth)
    -   [4.3 Soil Chemistries against
        Contour](#43-soil-chemistries-against-contour)
    -   [4.4 PCA for characterisation](#44-pca-for-characterisation)
-   [5 STATISTICAL ANALYSIS](#5-statistical-analysis)

------------------------------------------------------------------------

------------------------------------------------------------------------

# 1 R PACKAGES

``` r
library(tidyverse)
library(car)
library(kableExtra)
library(cowplot)
library(gridExtra)
library(factoextra)
library(FactoMineR)
```

# 2 DATA PREPARATION

## 2.1 Data Import

Randomly sample 10 rows of data from the table

``` r
sample_n(Soils, 10) %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("bordered", "stripped", "hover"), full_width = F)
```

<table class="table table-bordered table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">

Group

</th>
<th style="text-align:left;">

Contour

</th>
<th style="text-align:left;">

Depth

</th>
<th style="text-align:left;">

Gp

</th>
<th style="text-align:left;">

Block

</th>
<th style="text-align:right;">

pH

</th>
<th style="text-align:right;">

N

</th>
<th style="text-align:right;">

Dens

</th>
<th style="text-align:right;">

P

</th>
<th style="text-align:right;">

Ca

</th>
<th style="text-align:right;">

Mg

</th>
<th style="text-align:right;">

K

</th>
<th style="text-align:right;">

Na

</th>
<th style="text-align:right;">

Conduc

</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

21

</td>
<td style="text-align:left;">

6

</td>
<td style="text-align:left;">

Slope

</td>
<td style="text-align:left;">

10-30

</td>
<td style="text-align:left;">

S1

</td>
<td style="text-align:left;">

1

</td>
<td style="text-align:right;">

4.57

</td>
<td style="text-align:right;">

0.102

</td>
<td style="text-align:right;">

1.37

</td>
<td style="text-align:right;">

156

</td>
<td style="text-align:right;">

8.58

</td>
<td style="text-align:right;">

9.92

</td>
<td style="text-align:right;">

0.63

</td>
<td style="text-align:right;">

3.67

</td>
<td style="text-align:right;">

3.24

</td>
</tr>
<tr>
<td style="text-align:left;">

35

</td>
<td style="text-align:left;">

9

</td>
<td style="text-align:left;">

Depression

</td>
<td style="text-align:left;">

0-10

</td>
<td style="text-align:left;">

D0

</td>
<td style="text-align:left;">

3

</td>
<td style="text-align:right;">

5.30

</td>
<td style="text-align:right;">

0.136

</td>
<td style="text-align:right;">

1.00

</td>
<td style="text-align:right;">

259

</td>
<td style="text-align:right;">

9.96

</td>
<td style="text-align:right;">

8.08

</td>
<td style="text-align:right;">

0.45

</td>
<td style="text-align:right;">

1.97

</td>
<td style="text-align:right;">

2.27

</td>
</tr>
<tr>
<td style="text-align:left;">

39

</td>
<td style="text-align:left;">

10

</td>
<td style="text-align:left;">

Depression

</td>
<td style="text-align:left;">

10-30

</td>
<td style="text-align:left;">

D1

</td>
<td style="text-align:left;">

3

</td>
<td style="text-align:right;">

4.79

</td>
<td style="text-align:right;">

0.047

</td>
<td style="text-align:right;">

1.46

</td>
<td style="text-align:right;">

121

</td>
<td style="text-align:right;">

6.99

</td>
<td style="text-align:right;">

9.91

</td>
<td style="text-align:right;">

0.30

</td>
<td style="text-align:right;">

5.15

</td>
<td style="text-align:right;">

6.82

</td>
</tr>
<tr>
<td style="text-align:left;">

28

</td>
<td style="text-align:left;">

7

</td>
<td style="text-align:left;">

Slope

</td>
<td style="text-align:left;">

30-60

</td>
<td style="text-align:left;">

S3

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:right;">

4.99

</td>
<td style="text-align:right;">

0.048

</td>
<td style="text-align:right;">

1.46

</td>
<td style="text-align:right;">

97

</td>
<td style="text-align:right;">

7.49

</td>
<td style="text-align:right;">

9.38

</td>
<td style="text-align:right;">

0.40

</td>
<td style="text-align:right;">

9.70

</td>
<td style="text-align:right;">

9.13

</td>
</tr>
<tr>
<td style="text-align:left;">

12

</td>
<td style="text-align:left;">

3

</td>
<td style="text-align:left;">

Top

</td>
<td style="text-align:left;">

30-60

</td>
<td style="text-align:left;">

T3

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:right;">

3.89

</td>
<td style="text-align:right;">

0.070

</td>
<td style="text-align:right;">

1.42

</td>
<td style="text-align:right;">

117

</td>
<td style="text-align:right;">

6.61

</td>
<td style="text-align:right;">

9.76

</td>
<td style="text-align:right;">

0.41

</td>
<td style="text-align:right;">

8.30

</td>
<td style="text-align:right;">

9.21

</td>
</tr>
<tr>
<td style="text-align:left;">

18

</td>
<td style="text-align:left;">

5

</td>
<td style="text-align:left;">

Slope

</td>
<td style="text-align:left;">

0-10

</td>
<td style="text-align:left;">

S0

</td>
<td style="text-align:left;">

2

</td>
<td style="text-align:right;">

5.46

</td>
<td style="text-align:right;">

0.298

</td>
<td style="text-align:right;">

0.96

</td>
<td style="text-align:right;">

300

</td>
<td style="text-align:right;">

12.30

</td>
<td style="text-align:right;">

7.50

</td>
<td style="text-align:right;">

0.68

</td>
<td style="text-align:right;">

2.00

</td>
<td style="text-align:right;">

1.98

</td>
</tr>
<tr>
<td style="text-align:left;">

44

</td>
<td style="text-align:left;">

11

</td>
<td style="text-align:left;">

Depression

</td>
<td style="text-align:left;">

30-60

</td>
<td style="text-align:left;">

D3

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:right;">

4.64

</td>
<td style="text-align:right;">

0.065

</td>
<td style="text-align:right;">

1.46

</td>
<td style="text-align:right;">

152

</td>
<td style="text-align:right;">

4.43

</td>
<td style="text-align:right;">

10.54

</td>
<td style="text-align:right;">

0.22

</td>
<td style="text-align:right;">

7.61

</td>
<td style="text-align:right;">

9.09

</td>
</tr>
<tr>
<td style="text-align:left;">

33

</td>
<td style="text-align:left;">

9

</td>
<td style="text-align:left;">

Depression

</td>
<td style="text-align:left;">

0-10

</td>
<td style="text-align:left;">

D0

</td>
<td style="text-align:left;">

1

</td>
<td style="text-align:right;">

5.24

</td>
<td style="text-align:right;">

0.194

</td>
<td style="text-align:right;">

1.00

</td>
<td style="text-align:right;">

445

</td>
<td style="text-align:right;">

12.27

</td>
<td style="text-align:right;">

6.27

</td>
<td style="text-align:right;">

0.72

</td>
<td style="text-align:right;">

1.02

</td>
<td style="text-align:right;">

0.75

</td>
</tr>
<tr>
<td style="text-align:left;">

11

</td>
<td style="text-align:left;">

3

</td>
<td style="text-align:left;">

Top

</td>
<td style="text-align:left;">

30-60

</td>
<td style="text-align:left;">

T3

</td>
<td style="text-align:left;">

3

</td>
<td style="text-align:right;">

4.17

</td>
<td style="text-align:right;">

0.078

</td>
<td style="text-align:right;">

1.26

</td>
<td style="text-align:right;">

112

</td>
<td style="text-align:right;">

6.29

</td>
<td style="text-align:right;">

7.95

</td>
<td style="text-align:right;">

0.26

</td>
<td style="text-align:right;">

5.30

</td>
<td style="text-align:right;">

8.37

</td>
</tr>
<tr>
<td style="text-align:left;">

29

</td>
<td style="text-align:left;">

8

</td>
<td style="text-align:left;">

Slope

</td>
<td style="text-align:left;">

60-90

</td>
<td style="text-align:left;">

S6

</td>
<td style="text-align:left;">

1

</td>
<td style="text-align:right;">

3.80

</td>
<td style="text-align:right;">

0.049

</td>
<td style="text-align:right;">

1.48

</td>
<td style="text-align:right;">

108

</td>
<td style="text-align:right;">

3.82

</td>
<td style="text-align:right;">

8.80

</td>
<td style="text-align:right;">

0.24

</td>
<td style="text-align:right;">

9.57

</td>
<td style="text-align:right;">

11.57

</td>
</tr>
</tbody>
</table>

## 2.2 Data Description

``` r
Variables <- names(Soils)

Description <- c("a factor with 12 levels, corresponding to the combinations of Contour and Depth",
                 "a factor with 3 levels: Depression Slope Top",
                 "a factor with 4 levels: 0-10 10-30 30-60 60-90",
                 "a factor with 12 levels, giving abbreviations for the groups: D0 D1 D3 D6 S0 S1 S3 S6 T0 T1 T3 T6",
                 "a factor with levels 1 2 3 4",
                 "soil pH",
                 "total nitrogen in %",
                 "bulk density in gm/cm$^3$",
                 "total phosphorous in ppm",
                 "calcium in me/100 gm.",
                 "magnesium in me/100 gm.",
                 "phosphorous in me/100 gm.",
                 "sodium in me/100 gm.",
                 "conductivity Details")

data.frame(Variables, Description) %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("bordered", "stripped", "hover"), full_width = F)
```

<table class="table table-bordered table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">

Variables

</th>
<th style="text-align:left;">

Description

</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

Group

</td>
<td style="text-align:left;">

a factor with 12 levels, corresponding to the combinations of Contour
and Depth

</td>
</tr>
<tr>
<td style="text-align:left;">

Contour

</td>
<td style="text-align:left;">

a factor with 3 levels: Depression Slope Top

</td>
</tr>
<tr>
<td style="text-align:left;">

Depth

</td>
<td style="text-align:left;">

a factor with 4 levels: 0-10 10-30 30-60 60-90

</td>
</tr>
<tr>
<td style="text-align:left;">

Gp

</td>
<td style="text-align:left;">

a factor with 12 levels, giving abbreviations for the groups: D0 D1 D3
D6 S0 S1 S3 S6 T0 T1 T3 T6

</td>
</tr>
<tr>
<td style="text-align:left;">

Block

</td>
<td style="text-align:left;">

a factor with levels 1 2 3 4

</td>
</tr>
<tr>
<td style="text-align:left;">

pH

</td>
<td style="text-align:left;">

soil pH

</td>
</tr>
<tr>
<td style="text-align:left;">

N

</td>
<td style="text-align:left;">

total nitrogen in %

</td>
</tr>
<tr>
<td style="text-align:left;">

Dens

</td>
<td style="text-align:left;">

bulk density in
gm/cm![^3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5E3 "^3")

</td>
</tr>
<tr>
<td style="text-align:left;">

P

</td>
<td style="text-align:left;">

total phosphorous in ppm

</td>
</tr>
<tr>
<td style="text-align:left;">

Ca

</td>
<td style="text-align:left;">

calcium in me/100 gm.

</td>
</tr>
<tr>
<td style="text-align:left;">

Mg

</td>
<td style="text-align:left;">

magnesium in me/100 gm.

</td>
</tr>
<tr>
<td style="text-align:left;">

K

</td>
<td style="text-align:left;">

phosphorous in me/100 gm.

</td>
</tr>
<tr>
<td style="text-align:left;">

Na

</td>
<td style="text-align:left;">

sodium in me/100 gm.

</td>
</tr>
<tr>
<td style="text-align:left;">

Conduc

</td>
<td style="text-align:left;">

conductivity Details

</td>
</tr>
</tbody>
</table>

## 2.3 Data Exploration

-   This is a small data set with 48 rows of data and 14 variables.
-   There are 4 factor variables which are “Group”, “Contour”, “Depth”,
    “Gp”, and “Block”.  
-   There are 9 numerical variables which are “pH”, “N”, “Dens”, “P”,
    “Ca”, “Mg”, “K”, “Na”, and “Conduc”.

``` r
glimpse(Soils)
```

    ## Rows: 48
    ## Columns: 14
    ## $ Group   <fct> 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6,…
    ## $ Contour <fct> Top, Top, Top, Top, Top, Top, Top, Top, Top, Top, Top, Top, To…
    ## $ Depth   <fct> 0-10, 0-10, 0-10, 0-10, 10-30, 10-30, 10-30, 10-30, 30-60, 30-…
    ## $ Gp      <fct> T0, T0, T0, T0, T1, T1, T1, T1, T3, T3, T3, T3, T6, T6, T6, T6…
    ## $ Block   <fct> 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1,…
    ## $ pH      <dbl> 5.40, 5.65, 5.14, 5.14, 5.14, 5.10, 4.70, 4.46, 4.37, 4.39, 4.…
    ## $ N       <dbl> 0.188, 0.165, 0.260, 0.169, 0.164, 0.094, 0.100, 0.112, 0.112,…
    ## $ Dens    <dbl> 0.92, 1.04, 0.95, 1.10, 1.12, 1.22, 1.52, 1.47, 1.07, 1.54, 1.…
    ## $ P       <int> 215, 208, 300, 248, 174, 129, 117, 170, 121, 115, 112, 117, 12…
    ## $ Ca      <dbl> 16.35, 12.25, 13.02, 11.92, 14.17, 8.55, 8.74, 9.49, 8.85, 4.7…
    ## $ Mg      <dbl> 7.65, 5.15, 5.68, 7.88, 8.12, 6.92, 8.16, 9.16, 10.35, 6.91, 7…
    ## $ K       <dbl> 0.72, 0.71, 0.68, 1.09, 0.70, 0.81, 0.39, 0.70, 0.74, 0.77, 0.…
    ## $ Na      <dbl> 1.14, 0.94, 0.60, 1.01, 2.17, 2.67, 3.32, 3.76, 5.74, 5.85, 5.…
    ## $ Conduc  <dbl> 1.09, 1.35, 1.41, 1.64, 1.85, 3.18, 4.16, 5.14, 5.73, 6.45, 8.…

There is no missing value in the data set.

``` r
colSums(is.na(Soils))
```

    ##   Group Contour   Depth      Gp   Block      pH       N    Dens       P      Ca 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##      Mg       K      Na  Conduc 
    ##       0       0       0       0

Summarising the data:

``` r
summary(Soils, maxsum = 12)
```

    ##  Group        Contour     Depth     Gp    Block        pH       
    ##  1 :4   Depression:16   0-10 :12   D0:4   1:12   Min.   :3.740  
    ##  2 :4   Slope     :16   10-30:12   D1:4   2:12   1st Qu.:4.058  
    ##  3 :4   Top       :16   30-60:12   D3:4   3:12   Median :4.545  
    ##  4 :4                   60-90:12   D6:4   4:12   Mean   :4.669  
    ##  5 :4                              S0:4          3rd Qu.:5.140  
    ##  6 :4                              S1:4          Max.   :6.670  
    ##  7 :4                              S3:4                         
    ##  8 :4                              S6:4                         
    ##  9 :4                              T0:4                         
    ##  10:4                              T1:4                         
    ##  11:4                              T3:4                         
    ##  12:4                              T6:4                         
    ##        N                Dens             P               Ca        
    ##  Min.   :0.03000   Min.   :0.780   Min.   : 79.0   Min.   : 3.820  
    ##  1st Qu.:0.05075   1st Qu.:1.127   1st Qu.:108.8   1st Qu.: 5.040  
    ##  Median :0.08450   Median :1.400   Median :131.0   Median : 7.305  
    ##  Mean   :0.10194   Mean   :1.316   Mean   :166.2   Mean   : 8.029  
    ##  3rd Qu.:0.12925   3rd Qu.:1.502   3rd Qu.:214.2   3rd Qu.: 9.735  
    ##  Max.   :0.29800   Max.   :1.600   Max.   :445.0   Max.   :16.350  
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##        Mg               K                Na             Conduc      
    ##  Min.   : 5.150   Min.   :0.1400   Min.   : 0.600   Min.   : 0.670  
    ##  1st Qu.: 7.537   1st Qu.:0.2750   1st Qu.: 2.545   1st Qu.: 2.790  
    ##  Median : 8.515   Median :0.4250   Median : 5.520   Median : 6.635  
    ##  Mean   : 8.465   Mean   :0.4662   Mean   : 5.600   Mean   : 6.589  
    ##  3rd Qu.: 9.648   3rd Qu.:0.6425   3rd Qu.: 8.355   3rd Qu.: 9.852  
    ##  Max.   :10.960   Max.   :1.0900   Max.   :11.040   Max.   :13.320  
    ##                                                                     
    ##                                                                     
    ##                                                                     
    ##                                                                     
    ##                                                                     
    ## 

Based on the information in the variables “contour”, “depth”, and
“block.” I can see that the data are actually collected from 4 different
block (1 to 4), and there are 3 different Contour in each block, which
are “top”, “slope” and “depression”.

In each Contour of each block, soils are collected in 4 different soil
depth as described below.

![](https://raw.githubusercontent.com/KAR-NG/soil/main/pic1_block.png)

# 3 DATA CLEANING

## 3.1 Removing Group and Gp

Group and Gp are duplicates of each other, however, I will remove them
because they are not the best representation (though not bad) of the
information, from my experience as an environmental consultant
specialised in soil science. I will create an alternative of them if
required.

``` r
Soils <- Soils %>% dplyr::select(-Group, -Gp)
```

## 3.2 Rearrange

This step is optional. I am relocating “Block” to be the first column

``` r
Soils <- Soils %>% relocate(Block, .before = Contour)
```

## 3.3 Trimming

Remove leading and/or trailing white space from character strings.

``` r
Soils <- Soils %>% 
  mutate(Block = as.factor(trimws(Block)),
         Contour = as.factor(trimws(Contour)),
         Depth = as.factor(trimws(Depth)))  
```

## 3.4 Rearrange levels

Rearrange the sequence of levels in the factor variable “Contour” and
“Depth”, so that they are in the right orders during visualisation.

Current sequence of categories (technically known as “levels”) in the
variable “Contour”:

``` r
levels(Soils$Contour)
```

    ## [1] "Depression" "Slope"      "Top"

Current sequence of categories (technically known as “levels”) in the
variable “Depth”:

``` r
levels(Soils$Depth)
```

    ## [1] "0-10"  "10-30" "30-60" "60-90"

I will be reversing the order of these categories in “Contour” and
“Depth”.

Operation:

``` r
Soils <- Soils %>%
  mutate(Contour = fct_relevel(Contour, "Top", "Slope", "Depression"),
         Depth = fct_relevel(Depth, "60-90", "30-60", "10-30", "0-10"))
```

# 4 EXPLORATORY DATA ANALYSIS

``` r
soil.df <- Soils  %>% 
  pivot_longer(c(4:12), names_to = "chemistry", values_to = "result") %>% 
  mutate(chemistry = as.factor(chemistry),
         chemistry = fct_relevel(chemistry, "pH", "Conduc", "Dens", "N", "P", "K", "Na", "Ca", "Mg")) 
```

## 4.1 Univariate histogram

Following shows the graphs for each variables in the data set.

``` r
g1 <- soil.df %>% group_by(Block) %>% summarise(count = n()) %>% 
  ggplot(aes(x = Block, y = count)) + geom_bar(stat = "identity") + geom_label(aes(label = count)) +
  ggtitle("Sample sizes of Block Categories")

g2 <- soil.df %>% group_by(Contour) %>% summarise(count = n()) %>% 
  ggplot(aes(x = Contour, y = count)) + geom_bar(stat = "identity") + geom_label(aes(label = count)) +
  ggtitle("Sample sizes of Contour Categories")

g3 <- soil.df %>% group_by(Depth) %>% summarise(count = n()) %>% 
  ggplot(aes(x = Depth, y = count)) + geom_bar(stat = "identity") + geom_label(aes(label = count)) +
  ggtitle("Sample sizes of Depth Categories")

g4 <- ggplot(soil.df, aes(x = result, fill = chemistry)) +
  geom_histogram(color = "black") +
  facet_wrap(~chemistry, scales = "free") +
  ggtitle("Histogram")

top <- plot_grid(g1, g2, g3, ncol = 3)
plot_grid(top, g4, ncol = 1, rel_heights = c(1,2))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](soil_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> **Insights**

-   Based on the top bar chart, the sample size is balanced for all
    factor variables “Block”, “Contour”, and “Depth”.

-   All soils have slight to strongly acidic pH.

-   Many soils have high density

-   There is no obvious normal distribution in each histogram and it is
    likely due to the effect of different soil depth, block, and
    contour.

Let’s find it out.

## 4.2 Soil Chemistries against Soil Depth

-   pH, N, P, K and Ca tends to be higher in the topsoils.
-   Conductivity (Conduc), Soil density (Dens), Na, and Mg tends to be
    higher in the subsoils.

``` r
ggplot(soil.df, aes(x = result, y = Depth)) +
  geom_boxplot(outlier.shape = NA, shape = 21) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~chemistry, scales = "free") +
  stat_summary(fun = "mean", shape = 4, size = 3, geom = "point") +
  theme_bw() +
  theme(legend.position = "none")
```

![](soil_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## 4.3 Soil Chemistries against Contour

-   The relationship between all soil chemistry and soil depth are
    highly similar in 3 different contour.  
-   At the “top” contour, topsoil K and Ca can be slightly higher than
    the topsoils of other contours.  
-   At the “Slope” contour, topsoil average N is the highest compared to
    the topsoils of other contours.  
-   At the “depression” contour, topsoil P is the highest.

``` r
ggplot(soil.df, aes(x = result, y = Depth, colour = Contour)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5) +
  facet_grid(Contour~chemistry, scales = "free") +
  stat_summary(fun = "mean", shape = 4, size = 3, geom = "point") +
  theme_bw() +
  theme(legend.position = "bottom")
```

![](soil_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## 4.4 PCA for characterisation

Principal Component Analysis (PCA) is applied to understand the overall
trends in the data set.

Three different PCA plots are applied with data points grouped based on
ellipses of “Block”, “Contour”, “Depth”.

We can clearly see that data points are actually seperated based on soil
depth. Therefore, graph 3 should be emphasized for interpretation.

``` r
Soils
```

    ##    Block    Contour Depth   pH     N Dens   P    Ca    Mg    K    Na Conduc
    ## 1      1        Top  0-10 5.40 0.188 0.92 215 16.35  7.65 0.72  1.14   1.09
    ## 2      2        Top  0-10 5.65 0.165 1.04 208 12.25  5.15 0.71  0.94   1.35
    ## 3      3        Top  0-10 5.14 0.260 0.95 300 13.02  5.68 0.68  0.60   1.41
    ## 4      4        Top  0-10 5.14 0.169 1.10 248 11.92  7.88 1.09  1.01   1.64
    ## 5      1        Top 10-30 5.14 0.164 1.12 174 14.17  8.12 0.70  2.17   1.85
    ## 6      2        Top 10-30 5.10 0.094 1.22 129  8.55  6.92 0.81  2.67   3.18
    ## 7      3        Top 10-30 4.70 0.100 1.52 117  8.74  8.16 0.39  3.32   4.16
    ## 8      4        Top 10-30 4.46 0.112 1.47 170  9.49  9.16 0.70  3.76   5.14
    ## 9      1        Top 30-60 4.37 0.112 1.07 121  8.85 10.35 0.74  5.74   5.73
    ## 10     2        Top 30-60 4.39 0.058 1.54 115  4.73  6.91 0.77  5.85   6.45
    ## 11     3        Top 30-60 4.17 0.078 1.26 112  6.29  7.95 0.26  5.30   8.37
    ## 12     4        Top 30-60 3.89 0.070 1.42 117  6.61  9.76 0.41  8.30   9.21
    ## 13     1        Top 60-90 3.88 0.077 1.25 127  6.41 10.96 0.56  9.67  10.64
    ## 14     2        Top 60-90 4.07 0.046 1.54  91  3.82  6.61 0.50  7.67  10.07
    ## 15     3        Top 60-90 3.88 0.055 1.53  91  4.98  8.00 0.23  8.78  11.26
    ## 16     4        Top 60-90 3.74 0.053 1.40  79  5.86 10.14 0.41 11.04  12.15
    ## 17     1      Slope  0-10 5.11 0.247 0.94 261 13.25  7.55 0.61  1.86   2.61
    ## 18     2      Slope  0-10 5.46 0.298 0.96 300 12.30  7.50 0.68  2.00   1.98
    ## 19     3      Slope  0-10 5.61 0.145 1.10 242  9.66  6.76 0.63  1.01   0.76
    ## 20     4      Slope  0-10 5.85 0.186 1.20 229 13.78  7.12 0.62  3.09   2.85
    ## 21     1      Slope 10-30 4.57 0.102 1.37 156  8.58  9.92 0.63  3.67   3.24
    ## 22     2      Slope 10-30 5.11 0.097 1.30 139  8.58  8.69 0.42  4.70   4.63
    ## 23     3      Slope 10-30 4.78 0.122 1.30 214  8.22  7.75 0.32  3.07   3.67
    ## 24     4      Slope 10-30 6.67 0.083 1.42 132 12.68  9.56 0.55  8.30   8.10
    ## 25     1      Slope 30-60 3.96 0.059 1.53  98  4.80 10.00 0.36  6.52   7.72
    ## 26     2      Slope 30-60 4.00 0.050 1.50 115  5.06  8.91 0.28  7.91   9.78
    ## 27     3      Slope 30-60 4.12 0.086 1.55 148  6.16  7.58 0.16  6.39   9.07
    ## 28     4      Slope 30-60 4.99 0.048 1.46  97  7.49  9.38 0.40  9.70   9.13
    ## 29     1      Slope 60-90 3.80 0.049 1.48 108  3.82  8.80 0.24  9.57  11.57
    ## 30     2      Slope 60-90 3.96 0.036 1.28 103  4.78  7.29 0.24  9.67  11.42
    ## 31     3      Slope 60-90 3.93 0.048 1.42 109  4.93  7.47 0.14  9.65  13.32
    ## 32     4      Slope 60-90 4.02 0.039 1.51 100  5.66  8.84 0.37 10.54  11.57
    ## 33     1 Depression  0-10 5.24 0.194 1.00 445 12.27  6.27 0.72  1.02   0.75
    ## 34     2 Depression  0-10 5.20 0.256 0.78 380 11.39  7.55 0.78  1.63   2.20
    ## 35     3 Depression  0-10 5.30 0.136 1.00 259  9.96  8.08 0.45  1.97   2.27
    ## 36     4 Depression  0-10 5.67 0.127 1.13 248  9.12  7.04 0.55  1.43   0.67
    ## 37     1 Depression 10-30 4.46 0.087 1.24 276  7.24  9.40 0.43  4.17   5.08
    ## 38     2 Depression 10-30 4.91 0.092 1.47 158  7.37 10.57 0.59  5.07   6.37
    ## 39     3 Depression 10-30 4.79 0.047 1.46 121  6.99  9.91 0.30  5.15   6.82
    ## 40     4 Depression 10-30 5.36 0.095 1.26 195  8.59  8.66 0.48  4.17   3.65
    ## 41     1 Depression 30-60 3.94 0.054 1.60 148  4.85  9.62 0.18  7.20  10.14
    ## 42     2 Depression 30-60 4.52 0.051 1.53 115  6.34  9.78 0.34  8.52   9.74
    ## 43     3 Depression 30-60 4.35 0.032 1.55  82  5.99  9.73 0.22  7.02   8.60
    ## 44     4 Depression 30-60 4.64 0.065 1.46 152  4.43 10.54 0.22  7.61   9.09
    ## 45     1 Depression 60-90 3.82 0.038 1.40 105  4.65  9.85 0.18 10.15  12.26
    ## 46     2 Depression 60-90 4.24 0.035 1.47 100  4.56  8.95 0.33 10.51  11.29
    ## 47     3 Depression 60-90 4.22 0.030 1.56  97  5.29  8.37 0.14  8.27   9.51
    ## 48     4 Depression 60-90 4.41 0.058 1.58 130  4.58  9.46 0.14  9.28  12.69

``` r
pca.res <- PCA(Soils, quali.sup = c(1,2,3), graph = F)


fblock <- fviz_pca_biplot(pca.res, 
                repel = T,
                col.var = "black",
                habillage =  "Block",
                mean.point = F,
                addEllipses = T,
                ellipse.type = "convex") + labs(title = "Graph 1: PCA Biplot + Block Ellipse")

fcontour <- fviz_pca_biplot(pca.res, 
                repel = T,
                col.var = "black",
                habillage =  "Contour",
                mean.point = F,
                addEllipses = T,
                ellipse.type = "convex") + labs(title = "Graph 2: PCA Biplot + Contour Ellipse")

fdepth <- fviz_pca_biplot(pca.res, 
                repel = T,
                col.var = "black",
                habillage =  "Depth",
                mean.point = F,
                addEllipses = T,
                ellipse.type = "convex") + labs(title = "Graph 3: PCA Biplot + Depth Ellipse")


grid.arrange(fblock, fcontour, fdepth)
```

![](soil_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

**Characterisation from Graph 3**

-   First group identifiable is the cluster of Na, Dens (Density), and
    Conduc (Conductivity) are positively correlated to each other. Three
    of the variables will tend to increase together or decrease
    together.

-   Second group identifiable is the cluster of Na K, pH, Ca, N, and P
    are positively correlated to each other. Three of the variables will
    tend to increase together or decrease together.

-   First and the second group have negative relation, meaning any
    increase in a variable of a group will cause reduction of any
    variable in the opposite group. For example, when calcium level (Ca)
    increase, the sodium (Na) level will decrease because soils with
    high calcium generally has better structure and therefore less
    sodium level which is an element cause bad soil structure.

-   Topsoils are generally characterised by higher level of K, pH, Ca, N
    and P.

-   With increasing of soil depths, soils will experience higher level
    of Na, Density and conductivity with a trade off of K, pH, Ca, N and
    P reduction.

-   There is no interesting, clear trend of Mg in relation to soil
    depths and other soil chemistry.

# 5 STATISTICAL ANALYSIS

``` r
Soils  
```

    ##    Block    Contour Depth   pH     N Dens   P    Ca    Mg    K    Na Conduc
    ## 1      1        Top  0-10 5.40 0.188 0.92 215 16.35  7.65 0.72  1.14   1.09
    ## 2      2        Top  0-10 5.65 0.165 1.04 208 12.25  5.15 0.71  0.94   1.35
    ## 3      3        Top  0-10 5.14 0.260 0.95 300 13.02  5.68 0.68  0.60   1.41
    ## 4      4        Top  0-10 5.14 0.169 1.10 248 11.92  7.88 1.09  1.01   1.64
    ## 5      1        Top 10-30 5.14 0.164 1.12 174 14.17  8.12 0.70  2.17   1.85
    ## 6      2        Top 10-30 5.10 0.094 1.22 129  8.55  6.92 0.81  2.67   3.18
    ## 7      3        Top 10-30 4.70 0.100 1.52 117  8.74  8.16 0.39  3.32   4.16
    ## 8      4        Top 10-30 4.46 0.112 1.47 170  9.49  9.16 0.70  3.76   5.14
    ## 9      1        Top 30-60 4.37 0.112 1.07 121  8.85 10.35 0.74  5.74   5.73
    ## 10     2        Top 30-60 4.39 0.058 1.54 115  4.73  6.91 0.77  5.85   6.45
    ## 11     3        Top 30-60 4.17 0.078 1.26 112  6.29  7.95 0.26  5.30   8.37
    ## 12     4        Top 30-60 3.89 0.070 1.42 117  6.61  9.76 0.41  8.30   9.21
    ## 13     1        Top 60-90 3.88 0.077 1.25 127  6.41 10.96 0.56  9.67  10.64
    ## 14     2        Top 60-90 4.07 0.046 1.54  91  3.82  6.61 0.50  7.67  10.07
    ## 15     3        Top 60-90 3.88 0.055 1.53  91  4.98  8.00 0.23  8.78  11.26
    ## 16     4        Top 60-90 3.74 0.053 1.40  79  5.86 10.14 0.41 11.04  12.15
    ## 17     1      Slope  0-10 5.11 0.247 0.94 261 13.25  7.55 0.61  1.86   2.61
    ## 18     2      Slope  0-10 5.46 0.298 0.96 300 12.30  7.50 0.68  2.00   1.98
    ## 19     3      Slope  0-10 5.61 0.145 1.10 242  9.66  6.76 0.63  1.01   0.76
    ## 20     4      Slope  0-10 5.85 0.186 1.20 229 13.78  7.12 0.62  3.09   2.85
    ## 21     1      Slope 10-30 4.57 0.102 1.37 156  8.58  9.92 0.63  3.67   3.24
    ## 22     2      Slope 10-30 5.11 0.097 1.30 139  8.58  8.69 0.42  4.70   4.63
    ## 23     3      Slope 10-30 4.78 0.122 1.30 214  8.22  7.75 0.32  3.07   3.67
    ## 24     4      Slope 10-30 6.67 0.083 1.42 132 12.68  9.56 0.55  8.30   8.10
    ## 25     1      Slope 30-60 3.96 0.059 1.53  98  4.80 10.00 0.36  6.52   7.72
    ## 26     2      Slope 30-60 4.00 0.050 1.50 115  5.06  8.91 0.28  7.91   9.78
    ## 27     3      Slope 30-60 4.12 0.086 1.55 148  6.16  7.58 0.16  6.39   9.07
    ## 28     4      Slope 30-60 4.99 0.048 1.46  97  7.49  9.38 0.40  9.70   9.13
    ## 29     1      Slope 60-90 3.80 0.049 1.48 108  3.82  8.80 0.24  9.57  11.57
    ## 30     2      Slope 60-90 3.96 0.036 1.28 103  4.78  7.29 0.24  9.67  11.42
    ## 31     3      Slope 60-90 3.93 0.048 1.42 109  4.93  7.47 0.14  9.65  13.32
    ## 32     4      Slope 60-90 4.02 0.039 1.51 100  5.66  8.84 0.37 10.54  11.57
    ## 33     1 Depression  0-10 5.24 0.194 1.00 445 12.27  6.27 0.72  1.02   0.75
    ## 34     2 Depression  0-10 5.20 0.256 0.78 380 11.39  7.55 0.78  1.63   2.20
    ## 35     3 Depression  0-10 5.30 0.136 1.00 259  9.96  8.08 0.45  1.97   2.27
    ## 36     4 Depression  0-10 5.67 0.127 1.13 248  9.12  7.04 0.55  1.43   0.67
    ## 37     1 Depression 10-30 4.46 0.087 1.24 276  7.24  9.40 0.43  4.17   5.08
    ## 38     2 Depression 10-30 4.91 0.092 1.47 158  7.37 10.57 0.59  5.07   6.37
    ## 39     3 Depression 10-30 4.79 0.047 1.46 121  6.99  9.91 0.30  5.15   6.82
    ## 40     4 Depression 10-30 5.36 0.095 1.26 195  8.59  8.66 0.48  4.17   3.65
    ## 41     1 Depression 30-60 3.94 0.054 1.60 148  4.85  9.62 0.18  7.20  10.14
    ## 42     2 Depression 30-60 4.52 0.051 1.53 115  6.34  9.78 0.34  8.52   9.74
    ## 43     3 Depression 30-60 4.35 0.032 1.55  82  5.99  9.73 0.22  7.02   8.60
    ## 44     4 Depression 30-60 4.64 0.065 1.46 152  4.43 10.54 0.22  7.61   9.09
    ## 45     1 Depression 60-90 3.82 0.038 1.40 105  4.65  9.85 0.18 10.15  12.26
    ## 46     2 Depression 60-90 4.24 0.035 1.47 100  4.56  8.95 0.33 10.51  11.29
    ## 47     3 Depression 60-90 4.22 0.030 1.56  97  5.29  8.37 0.14  8.27   9.51
    ## 48     4 Depression 60-90 4.41 0.058 1.58 130  4.58  9.46 0.14  9.28  12.69
