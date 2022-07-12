Soil Profile Data Mining and Statistical Analysis
================
Kar
2022-07

-   [1 R PACKAGES](#1-r-packages)
-   [2 DATA PREPARATION](#2-data-preparation)
    -   [2.1 Data Import](#21-data-import)
    -   [2.2 Data Description](#22-data-description)
    -   [2.3 Data Exploration](#23-data-exploration)

------------------------------------------------------------------------

------------------------------------------------------------------------

# 1 R PACKAGES

``` r
library(tidyverse)
library(car)
library(kableExtra)
library(cowplot)
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

25

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

1

</td>
<td style="text-align:right;">

3.96

</td>
<td style="text-align:right;">

0.059

</td>
<td style="text-align:right;">

1.53

</td>
<td style="text-align:right;">

98

</td>
<td style="text-align:right;">

4.80

</td>
<td style="text-align:right;">

10.00

</td>
<td style="text-align:right;">

0.36

</td>
<td style="text-align:right;">

6.52

</td>
<td style="text-align:right;">

7.72

</td>
</tr>
<tr>
<td style="text-align:left;">

13

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:left;">

Top

</td>
<td style="text-align:left;">

60-90

</td>
<td style="text-align:left;">

T6

</td>
<td style="text-align:left;">

1

</td>
<td style="text-align:right;">

3.88

</td>
<td style="text-align:right;">

0.077

</td>
<td style="text-align:right;">

1.25

</td>
<td style="text-align:right;">

127

</td>
<td style="text-align:right;">

6.41

</td>
<td style="text-align:right;">

10.96

</td>
<td style="text-align:right;">

0.56

</td>
<td style="text-align:right;">

9.67

</td>
<td style="text-align:right;">

10.64

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

9

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

1

</td>
<td style="text-align:right;">

4.37

</td>
<td style="text-align:right;">

0.112

</td>
<td style="text-align:right;">

1.07

</td>
<td style="text-align:right;">

121

</td>
<td style="text-align:right;">

8.85

</td>
<td style="text-align:right;">

10.35

</td>
<td style="text-align:right;">

0.74

</td>
<td style="text-align:right;">

5.74

</td>
<td style="text-align:right;">

5.73

</td>
</tr>
<tr>
<td style="text-align:left;">

45

</td>
<td style="text-align:left;">

12

</td>
<td style="text-align:left;">

Depression

</td>
<td style="text-align:left;">

60-90

</td>
<td style="text-align:left;">

D6

</td>
<td style="text-align:left;">

1

</td>
<td style="text-align:right;">

3.82

</td>
<td style="text-align:right;">

0.038

</td>
<td style="text-align:right;">

1.40

</td>
<td style="text-align:right;">

105

</td>
<td style="text-align:right;">

4.65

</td>
<td style="text-align:right;">

9.85

</td>
<td style="text-align:right;">

0.18

</td>
<td style="text-align:right;">

10.15

</td>
<td style="text-align:right;">

12.26

</td>
</tr>
<tr>
<td style="text-align:left;">

16

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:left;">

Top

</td>
<td style="text-align:left;">

60-90

</td>
<td style="text-align:left;">

T6

</td>
<td style="text-align:left;">

4

</td>
<td style="text-align:right;">

3.74

</td>
<td style="text-align:right;">

0.053

</td>
<td style="text-align:right;">

1.40

</td>
<td style="text-align:right;">

79

</td>
<td style="text-align:right;">

5.86

</td>
<td style="text-align:right;">

10.14

</td>
<td style="text-align:right;">

0.41

</td>
<td style="text-align:right;">

11.04

</td>
<td style="text-align:right;">

12.15

</td>
</tr>
<tr>
<td style="text-align:left;">

17

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

1

</td>
<td style="text-align:right;">

5.11

</td>
<td style="text-align:right;">

0.247

</td>
<td style="text-align:right;">

0.94

</td>
<td style="text-align:right;">

261

</td>
<td style="text-align:right;">

13.25

</td>
<td style="text-align:right;">

7.55

</td>
<td style="text-align:right;">

0.61

</td>
<td style="text-align:right;">

1.86

</td>
<td style="text-align:right;">

2.61

</td>
</tr>
<tr>
<td style="text-align:left;">

36

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

4

</td>
<td style="text-align:right;">

5.67

</td>
<td style="text-align:right;">

0.127

</td>
<td style="text-align:right;">

1.13

</td>
<td style="text-align:right;">

248

</td>
<td style="text-align:right;">

9.12

</td>
<td style="text-align:right;">

7.04

</td>
<td style="text-align:right;">

0.55

</td>
<td style="text-align:right;">

1.43

</td>
<td style="text-align:right;">

0.67

</td>
</tr>
<tr>
<td style="text-align:left;">

22

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

2

</td>
<td style="text-align:right;">

5.11

</td>
<td style="text-align:right;">

0.097

</td>
<td style="text-align:right;">

1.30

</td>
<td style="text-align:right;">

139

</td>
<td style="text-align:right;">

8.58

</td>
<td style="text-align:right;">

8.69

</td>
<td style="text-align:right;">

0.42

</td>
<td style="text-align:right;">

4.70

</td>
<td style="text-align:right;">

4.63

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
