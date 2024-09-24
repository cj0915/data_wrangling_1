Tidy_Data
================
Chenyu
2024-09-24

This doc will show how to tidy data

## pivot_longer

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(haven)
pulse_df = 
  read_sas("data_import_examples/data_import_examples/public_pulse_data.sas7bdat") |>
  janitor::clean_names()
```

This needs to go from wide to long format.

``` r
pulse_tidy_df = 
  pulse_df |>
  pivot_longer(
    cols = bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi_score",
    names_prefix = "bdi_score_"
  ) |> 
  mutate(
    visit = replace(visit, visit == "bl", "00m")
  ) |>
  relocate(id, visit)
```

Do one more example.

``` r
litters_df = 
  read.csv("data_import_examples/data_import_examples/FAS_litters.csv", 
           na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  select(litter_number, ends_with("weight")) |> 
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd", 
    values_to = "weight") |> 
  mutate(
    gd = case_match(
      gd,
      "gd0_weight"  ~ 0,
      "gd18_weight" ~ 18
    ))
```

## Pivot wider

Let’s make up an analysis result table.

``` r
analysis_result = 
  tibble(
    group = c("treatment", "treatment", "placebo", "placebo"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 8, 3.5, 4)
  )

analysis_result
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

``` r
analysis_result|>
  pivot_wider(
  names_from = "time", 
  values_from = "mean") |>
  knitr::kable()
```

| group     | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |    8 |
| placebo   | 3.5 |    4 |

## Binding tables.

``` r
fellowship_ring = 
  readxl::read_excel("data_import_examples/data_import_examples/LotR_Words.xlsx", 
                     range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("data_import_examples/data_import_examples/LotR_Words.xlsx", 
                     range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("data_import_examples/data_import_examples/LotR_Words.xlsx", 
                     range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words") |>
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

## Join FAS datasets

Import ‘litters’ dataset,

``` r
litters_df = 
  read.csv("data_import_examples/data_import_examples/FAS_litters.csv", 
           na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  separate(group, into = c("dose", "day_of_tx"), sep = 3) |>
  relocate(litter_number) |>
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose))
```

Import ‘pups’ dataset,

``` r
pups_df = 
  read.csv("data_import_examples/data_import_examples/FAS_pups.csv", 
           na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  mutate(
    sex = case_match(
        sex, 
        1 ~ "male", 
        2 ~ "female"),
    sex = as.factor(sex)) 
```

Join the datasets!

``` r
fas_df = 
  left_join(pups_df, litters_df, by = "litter_number") |>
  relocate(litter_number, dose, day_of_tx)

fas_df
```

    ##       litter_number dose day_of_tx    sex pd_ears pd_eyes pd_pivot pd_walk
    ## 1               #85  con         7   male       4      13        7      11
    ## 2               #85  con         7   male       4      13        7      12
    ## 3         #1/2/95/2  con         7   male       5      13        7       9
    ## 4         #1/2/95/2  con         7   male       5      13        8      10
    ## 5     #5/5/3/83/3-3  con         7   male       5      13        8      10
    ## 6     #5/5/3/83/3-3  con         7   male       5      14        6       9
    ## 7       #5/4/2/95/2  con         7   male      NA      14        5       9
    ## 8       #4/2/95/3-3  con         7   male       4      13        6       8
    ## 9       #4/2/95/3-3  con         7   male       4      13        7       9
    ## 10      #2/2/95/3-2  con         7   male       4      NA        8      10
    ## 11  #1/5/3/83/3-3/2  con         7   male       4      NA       NA       9
    ## 12  #1/5/3/83/3-3/2  con         7   male       4      NA        7       9
    ## 13  #1/5/3/83/3-3/2  con         7   male       4      NA        7       9
    ## 14  #1/5/3/83/3-3/2  con         7   male       4      NA        7       9
    ## 15  #1/5/3/83/3-3/2  con         7   male       4      NA        7       9
    ## 16              #85  con         7 female       4      13        6      11
    ## 17        #1/2/95/2  con         7 female       4      13        7       9
    ## 18        #1/2/95/2  con         7 female       4      13        7      10
    ## 19        #1/2/95/2  con         7 female       5      13        8      10
    ## 20        #1/2/95/2  con         7 female       5      13        8      10
    ## 21        #1/2/95/2  con         7 female       5      13        6      10
    ## 22    #5/5/3/83/3-3  con         7 female       5      13        8      10
    ## 23    #5/5/3/83/3-3  con         7 female       5      14        7      10
    ## 24    #5/5/3/83/3-3  con         7 female       5      14        8      10
    ## 25      #5/4/2/95/2  con         7 female      NA      14        7      10
    ## 26      #5/4/2/95/2  con         7 female      NA      14        7      10
    ## 27      #5/4/2/95/2  con         7 female      NA      14        7      10
    ## 28      #4/2/95/3-3  con         7 female       4      13        5       7
    ## 29      #4/2/95/3-3  con         7 female       4      13        7       9
    ## 30      #4/2/95/3-3  con         7 female       4      13        6       8
    ## 31      #4/2/95/3-3  con         7 female       4      13        7       9
    ## 32      #2/2/95/3-2  con         7 female       4      NA        7      10
    ## 33      #2/2/95/3-2  con         7 female       4      NA        8      10
    ## 34      #2/2/95/3-2  con         7 female       4      NA        8      11
    ## 35  #1/5/3/83/3-3/2  con         7 female       4      NA        7       9
    ## 36  #1/5/3/83/3-3/2  con         7 female       4      NA        7       9
    ## 37  #1/5/3/83/3-3/2  con         7 female       4      NA        7       9
    ## 38  #1/5/3/83/3-3/2  con         7 female       4      NA        7       9
    ## 39        #3/83/3-3  con         8   male       3      13        4       7
    ## 40        #3/83/3-3  con         8   male       3      13       NA       7
    ## 41        #3/83/3-3  con         8   male       3      13        5       7
    ## 42        #3/83/3-3  con         8   male       3      12        5       8
    ## 43        #3/83/3-3  con         8   male       4      13        7       9
    ## 44          #2/95/3  con         8   male       4      13        6       9
    ## 45          #2/95/3  con         8   male       3      13        4       8
    ## 46          #2/95/3  con         8   male       3      13        4       8
    ## 47          #2/95/3  con         8   male       3      12        6       8
    ## 48      #3/5/2/2/95  con         8   male       4      13        6       8
    ## 49      #3/5/2/2/95  con         8   male       4      13        5       8
    ## 50      #3/5/2/2/95  con         8   male       4      13        5       8
    ## 51      #3/5/2/2/95  con         8   male       4      13        5       8
    ## 52      #5/4/3/83/3  con         8   male       4      13        7      10
    ## 53      #5/4/3/83/3  con         8   male       4      14        7       9
    ## 54      #5/4/3/83/3  con         8   male       4      14        6       9
    ## 55      #5/4/3/83/3  con         8   male       4      14        6      10
    ## 56      #5/4/3/83/3  con         8   male       4      14        7       9
    ## 57    #1/6/2/2/95-2  con         8   male       3      13        7       9
    ## 58    #1/6/2/2/95-2  con         8   male       4      13        7       9
    ## 59  #3/5/3/83/3-3-2  con         8   male       4      13        8      10
    ## 60  #3/5/3/83/3-3-2  con         8   male       4      13        7       9
    ## 61  #3/5/3/83/3-3-2  con         8   male       4      13        8      10
    ## 62  #3/5/3/83/3-3-2  con         8   male       4      13        8      10
    ## 63        #2/2/95/2  con         8   male       5      14        7       9
    ## 64        #2/2/95/2  con         8   male       4      14        8      11
    ## 65    #3/6/2/2/95-3  con         8   male       3      13        7       9
    ## 66    #3/6/2/2/95-3  con         8   male       3      13        7       9
    ## 67    #3/6/2/2/95-3  con         8   male       3      13        6       8
    ## 68    #3/6/2/2/95-3  con         8   male       3      12        6       8
    ## 69    #3/6/2/2/95-3  con         8   male       3      14        6       8
    ## 70        #3/83/3-3  con         8 female       3      13       NA       8
    ## 71        #3/83/3-3  con         8 female       3      13        4       7
    ## 72        #3/83/3-3  con         8 female       3      12        6       8
    ## 73          #2/95/3  con         8 female       3      12        6       9
    ## 74          #2/95/3  con         8 female       3      13        6       8
    ## 75          #2/95/3  con         8 female       4      12        6       9
    ## 76          #2/95/3  con         8 female       3      13        6       8
    ## 77      #3/5/2/2/95  con         8 female       4      13        6       9
    ## 78      #3/5/2/2/95  con         8 female       4      12        5       7
    ## 79      #3/5/2/2/95  con         8 female       3      13        5       9
    ## 80      #3/5/2/2/95  con         8 female       4      13        5       9
    ## 81      #5/4/3/83/3  con         8 female       4      13        7       9
    ## 82      #5/4/3/83/3  con         8 female       4      13        7      10
    ## 83      #5/4/3/83/3  con         8 female       4      13        7       9
    ## 84    #1/6/2/2/95-2  con         8 female       3      13        7       9
    ## 85    #1/6/2/2/95-2  con         8 female       3      13        5       8
    ## 86    #1/6/2/2/95-2  con         8 female       4      13        7       9
    ## 87    #1/6/2/2/95-2  con         8 female       4      13        8      10
    ## 88  #3/5/3/83/3-3-2  con         8 female       4      13        4       9
    ## 89  #3/5/3/83/3-3-2  con         8 female       4      13        7       9
    ## 90  #3/5/3/83/3-3-2  con         8 female       4      13        7      10
    ## 91  #3/5/3/83/3-3-2  con         8 female       4      13        7       9
    ## 92        #2/2/95/2  con         8 female       4      13        6       8
    ## 93        #2/2/95/2  con         8 female       4      13        9      11
    ## 94    #3/6/2/2/95-3  con         8 female       3      12        6       8
    ## 95    #3/6/2/2/95-3  con         8 female       3      12        7       9
    ## 96            #84/2  low         7   male       3      13        5       8
    ## 97            #84/2  low         7   male       3      13        7      10
    ## 98            #84/2  low         7   male       3      13        4       7
    ## 99             #107  low         7   male       4      13        9      11
    ## 100            #107  low         7   male       4      13        9      11
    ## 101            #107  low         7   male       4      13       10      12
    ## 102            #107  low         7   male       4      13        9      11
    ## 103            #107  low         7   male       4      13        9      11
    ## 104            #107  low         7   male       4      13        9      11
    ## 105            #107  low         7   male       4      13       10      12
    ## 106           #85/2  low         7   male       4      13        9      11
    ## 107           #85/2  low         7   male       4      13       10      12
    ## 108             #98  low         7   male       3      13        7      10
    ## 109             #98  low         7   male       4      13        9      11
    ## 110             #98  low         7   male       4      13        9      11
    ## 111             #98  low         7   male       4      13       NA      10
    ## 112             #98  low         7   male       3      13        9      11
    ## 113            #102  low         7   male       4      13        7      11
    ## 114            #102  low         7   male       4      13        9      11
    ## 115            #101  low         7   male       3      12       10      12
    ## 116            #101  low         7   male       3      13        7       9
    ## 117            #101  low         7   male       4      12        6       8
    ## 118            #101  low         7   male       4      12        6      11
    ## 119            #111  low         7   male       4      13        5      10
    ## 120           #84/2  low         7 female       3      13        5      12
    ## 121           #84/2  low         7 female       3      13        6       8
    ## 122           #84/2  low         7 female       3      12        8      10
    ## 123           #84/2  low         7 female       3      13        5       8
    ## 124           #84/2  low         7 female       3      13        9      11
    ## 125            #107  low         7 female       4      13        8      10
    ## 126           #85/2  low         7 female       4      12        9      11
    ## 127           #85/2  low         7 female       4      13       10      12
    ## 128           #85/2  low         7 female       4      13        9      11
    ## 129           #85/2  low         7 female       4      13       10      12
    ## 130             #98  low         7 female       2      13        7      10
    ## 131             #98  low         7 female       4      13        9      11
    ## 132             #98  low         7 female       4      13        7      10
    ## 133             #98  low         7 female       3      13        9      11
    ## 134            #102  low         7 female       4      14        9      11
    ## 135            #102  low         7 female       3      13        8      11
    ## 136            #102  low         7 female       3      13        9      11
    ## 137            #102  low         7 female       4      13        8      10
    ## 138            #102  low         7 female       3      13        9      11
    ## 139            #101  low         7 female       3      12        9      12
    ## 140            #101  low         7 female       3      14        9      11
    ## 141            #101  low         7 female       3      12        6      10
    ## 142            #101  low         7 female       4      12        9      11
    ## 143            #101  low         7 female       4      12        8      11
    ## 144            #111  low         7 female       4      13        5      10
    ## 145            #111  low         7 female       4      13        5      10
    ## 146             #59  mod         7   male       4      14       10      12
    ## 147             #59  mod         7   male       4      14        8      11
    ## 148             #59  mod         7   male       4      13       12      12
    ## 149            #103  mod         7   male       4      13        8      10
    ## 150            #103  mod         7   male       4      14        7       9
    ## 151            #103  mod         7   male       3      13        8      10
    ## 152       #1/82/3-2  mod         7   male       4      13        5      10
    ## 153       #1/82/3-2  mod         7   male       4      13       NA       8
    ## 154       #3/83/3-2  mod         7   male       4      13        6       9
    ## 155       #3/83/3-2  mod         7   male       4      13        6       9
    ## 156       #3/83/3-2  mod         7   male       4      13        6       8
    ## 157       #3/83/3-2  mod         7   male       4      13        6       8
    ## 158       #3/83/3-2  mod         7   male       4      13        6       8
    ## 159       #3/83/3-2  mod         7   male       4      12        6       9
    ## 160       #2/95/2-2  mod         7   male       4      13        6       8
    ## 161       #2/95/2-2  mod         7   male       4      13        6       8
    ## 162       #2/95/2-2  mod         7   male       4      13        6       8
    ## 163       #2/95/2-2  mod         7   male       4      13        7       9
    ## 164       #3/82/3-2  mod         7   male       3      13        6       8
    ## 165       #3/82/3-2  mod         7   male       4      13        6       8
    ## 166       #4/2/95/2  mod         7   male       4      14        8      10
    ## 167       #4/2/95/2  mod         7   male       4      14       NA      11
    ## 168       #4/2/95/2  mod         7   male       4      14       NA      11
    ## 169       #4/2/95/2  mod         7   male       4      14        7       9
    ## 170     #5/3/83/5-2  mod         7   male       4      13        6      10
    ## 171     #5/3/83/5-2  mod         7   male       4      14       NA      10
    ## 172     #5/3/83/5-2  mod         7   male       4      14        4      10
    ## 173      #8/110/3-2  mod         7   male       3      13        6       8
    ## 174      #8/110/3-2  mod         7   male       3      13        6       8
    ## 175      #8/110/3-2  mod         7   male       3      13        6       8
    ## 176      #8/110/3-2  mod         7   male       4      13        6       8
    ## 177            #106  mod         7   male       3      13       10      12
    ## 178           #94/2  mod         7   male      NA      13       NA       9
    ## 179             #62  mod         7   male       5      14       11      13
    ## 180             #62  mod         7   male       5      15       10      12
    ## 181             #62  mod         7   male       5      15       11      13
    ## 182             #59  mod         7 female       4      13       10      12
    ## 183             #59  mod         7 female       4      13        8      10
    ## 184            #103  mod         7 female       3      13        7       9
    ## 185            #103  mod         7 female       3      12        7       9
    ## 186            #103  mod         7 female       3      13        7       9
    ## 187            #103  mod         7 female       3      12        7       9
    ## 188            #103  mod         7 female       3      13        8      10
    ## 189            #103  mod         7 female       4      13        6       9
    ## 190       #1/82/3-2  mod         7 female       4      13        8      10
    ## 191       #1/82/3-2  mod         7 female       4      13        5      10
    ## 192       #1/82/3-2  mod         7 female       5      13        6      10
    ## 193       #1/82/3-2  mod         7 female       4      13        8      10
    ## 194       #3/83/3-2  mod         7 female       4      12       NA       8
    ## 195       #3/83/3-2  mod         7 female       4      12        6       8
    ## 196       #2/95/2-2  mod         7 female       4      13        6       9
    ## 197       #2/95/2-2  mod         7 female       4      13        6       8
    ## 198       #2/95/2-2  mod         7 female       4      13        5       8
    ## 199       #3/82/3-2  mod         7 female       4      13        5       8
    ## 200       #3/82/3-2  mod         7 female       3      12        4       8
    ## 201       #3/82/3-2  mod         7 female       3      12        6       8
    ## 202       #4/2/95/2  mod         7 female       4      14        8      11
    ## 203       #4/2/95/2  mod         7 female       4      14        7       9
    ## 204       #4/2/95/2  mod         7 female       4      14        7      10
    ## 205     #5/3/83/3-2 <NA>      <NA> female       3      12       NA       8
    ## 206     #5/3/83/3-2 <NA>      <NA> female       3      13       NA      10
    ## 207      #8/110/3-2  mod         7 female       3      12        4       9
    ## 208      #8/110/3-2  mod         7 female       3      13        6       8
    ## 209      #8/110/3-2  mod         7 female       4      14        6       9
    ## 210      #8/110/3-2  mod         7 female       4      13        6       8
    ## 211      #8/110/3-2  mod         7 female       4      13        6       8
    ## 212            #106  mod         7 female       3      14        8      10
    ## 213           #94/2  mod         7 female      NA      14       11      13
    ## 214           #94/2  mod         7 female      NA      13       NA       9
    ## 215             #62  mod         7 female       5      13       10      12
    ## 216             #53  low         8   male       4      13       10      12
    ## 217             #53  low         8   male       3      13        9      12
    ## 218             #53  low         8   male       4      13        8      12
    ## 219             #53  low         8   male       3      13       10      12
    ## 220             #53  low         8   male       4      13        9      11
    ## 221             #79  low         8   male       4      14        9      11
    ## 222             #79  low         8   male       4      14       12      14
    ## 223             #79  low         8   male       4      14        8      10
    ## 224             #79  low         8   male       4      14        6       9
    ## 225             #79  low         8   male       4      14       10      13
    ## 226            #100  low         8   male       3      13        7       9
    ## 227            #100  low         8   male       3      13        8      10
    ## 228           #4/84  low         8   male       3      13        7       9
    ## 229           #4/84  low         8   male       3      13        6      10
    ## 230           #4/84  low         8   male       4      13        7      10
    ## 231            #108  low         8   male       3      13        5       7
    ## 232            #108  low         8   male       3      12        6       8
    ## 233            #108  low         8   male       3      13        6       8
    ## 234            #108  low         8   male       3      13        6       8
    ## 235             #99  low         8   male      NA      12        8      10
    ## 236             #99  low         8   male      NA      13        7       9
    ## 237             #99  low         8   male      NA      13        5       9
    ## 238             #99  low         8   male      NA      12        6       9
    ## 239            #110  low         8   male      NA      12        6       8
    ## 240             #53  low         8 female       3      13       11      13
    ## 241             #53  low         8 female       4      13       10      12
    ## 242             #79  low         8 female       4      13        9      11
    ## 243             #79  low         8 female       4      14       12      14
    ## 244            #100  low         8 female       4      13        9      11
    ## 245            #100  low         8 female       3      13        9      11
    ## 246            #100  low         8 female       3      12        9      11
    ## 247            #100  low         8 female       3      13        8      10
    ## 248            #100  low         8 female       4      12        9      11
    ## 249           #4/84  low         8 female       3      13        6      10
    ## 250            #108  low         8 female       3      13        6       8
    ## 251            #108  low         8 female       3      14        6       8
    ## 252            #108  low         8 female       3      13        6      10
    ## 253             #99  low         8 female      NA      13        7       9
    ## 254            #110  low         8 female      NA      12        7       9
    ## 255            #110  low         8 female      NA      12        6       8
    ## 256            #110  low         8 female      NA      12        7       9
    ## 257            #110  low         8 female      NA      12        7       9
    ## 258            #110  low         8 female      NA      12        7       9
    ## 259             #97  mod         8   male       3      12        7       9
    ## 260             #97  mod         8   male       3      12        6       8
    ## 261             #97  mod         8   male       3      12        6       9
    ## 262             #97  mod         8   male       3      12        7       9
    ## 263             #97  mod         8   male       3      12        7       9
    ## 264             #97  mod         8   male       3      12        7       9
    ## 265           #5/93  mod         8   male       3      12        8      10
    ## 266           #5/93  mod         8   male       3      13        7       9
    ## 267           #5/93  mod         8   male       3      13        7       9
    ## 268         #5/93/2  mod         8   male       4      13        7       9
    ## 269       #7/82/3-2 <NA>      <NA>   male       3      12        6       8
    ## 270       #7/82/3-2 <NA>      <NA>   male       4      13        5       8
    ## 271       #7/82/3-2 <NA>      <NA>   male       3      13        6       8
    ## 272      #7/110/3-2  mod         8   male       3      14        8      10
    ## 273      #7/110/3-2  mod         8   male       3      14        8      10
    ## 274      #7/110/3-2  mod         8   male       4      14        8      10
    ## 275      #7/110/3-2  mod         8   male       3      14        7      10
    ## 276      #7/110/3-2  mod         8   male       3      14        8      10
    ## 277      #7/110/3-2  mod         8   male       3      14        8      10
    ## 278         #2/95/2  mod         8   male       4      13        7       9
    ## 279         #2/95/2  mod         8   male       4      13        7       9
    ## 280         #2/95/2  mod         8   male       4      13        7       9
    ## 281           #82/4  mod         8   male       4      13        8      10
    ## 282           #82/4  mod         8   male       3      13        7       9
    ## 283           #82/4  mod         8   male       4      13        7       9
    ## 284             #97  mod         8 female       3      12        7       9
    ## 285             #97  mod         8 female       3      12        6       8
    ## 286           #5/93  mod         8 female       4      13        7       9
    ## 287           #5/93  mod         8 female       3      12        7       9
    ## 288           #5/93  mod         8 female       4      13        7       9
    ## 289           #5/93  mod         8 female       3      13        7       9
    ## 290           #5/93  mod         8 female       3      12        7       9
    ## 291           #5/93  mod         8 female       3      12        7       9
    ## 292         #5/93/2  mod         8 female       4      14        7       9
    ## 293         #5/93/2  mod         8 female       5      14        7       9
    ## 294         #5/93/2  mod         8 female       4      13        7       9
    ## 295         #5/93/2  mod         8 female       5      14        7       9
    ## 296         #5/93/2  mod         8 female       4      13        7       9
    ## 297         #5/93/2  mod         8 female       4      14        6       9
    ## 298         #5/93/2  mod         8 female       5      13        7       9
    ## 299       #7/82/3-2 <NA>      <NA> female       3      13        6       8
    ## 300       #7/82/3-2 <NA>      <NA> female       3      12        6       8
    ## 301       #7/82/3-2 <NA>      <NA> female       3      12        6       8
    ## 302       #7/82/3-2 <NA>      <NA> female       3      12        6       8
    ## 303      #7/110/3-2  mod         8 female       4      14        8      10
    ## 304      #7/110/3-2  mod         8 female       4      14        7       9
    ## 305         #2/95/2  mod         8 female       4      12        7       9
    ## 306         #2/95/2  mod         8 female       4      12        6       8
    ## 307         #2/95/2  mod         8 female       4      13        7       9
    ## 308         #2/95/2  mod         8 female       4      12        7       9
    ## 309         #2/95/2  mod         8 female       3      13        6       8
    ## 310         #2/95/2  mod         8 female       3      13        7       9
    ## 311           #82/4  mod         8 female       4      13        7       9
    ## 312           #82/4  mod         8 female       3      13        7       9
    ## 313           #82/4  mod         8 female       3      13        7       9
    ##     gd0_weight gd18_weight gd_of_birth pups_born_alive pups_dead_birth
    ## 1         19.7        34.7          20               3               4
    ## 2         19.7        34.7          20               3               4
    ## 3         27.0        42.0          19               8               0
    ## 4         27.0        42.0          19               8               0
    ## 5         26.0        41.4          19               6               0
    ## 6         26.0        41.4          19               6               0
    ## 7         28.5        44.1          19               5               1
    ## 8           NA          NA          20               6               0
    ## 9           NA          NA          20               6               0
    ## 10          NA          NA          20               6               0
    ## 11          NA          NA          20               9               0
    ## 12          NA          NA          20               9               0
    ## 13          NA          NA          20               9               0
    ## 14          NA          NA          20               9               0
    ## 15          NA          NA          20               9               0
    ## 16        19.7        34.7          20               3               4
    ## 17        27.0        42.0          19               8               0
    ## 18        27.0        42.0          19               8               0
    ## 19        27.0        42.0          19               8               0
    ## 20        27.0        42.0          19               8               0
    ## 21        27.0        42.0          19               8               0
    ## 22        26.0        41.4          19               6               0
    ## 23        26.0        41.4          19               6               0
    ## 24        26.0        41.4          19               6               0
    ## 25        28.5        44.1          19               5               1
    ## 26        28.5        44.1          19               5               1
    ## 27        28.5        44.1          19               5               1
    ## 28          NA          NA          20               6               0
    ## 29          NA          NA          20               6               0
    ## 30          NA          NA          20               6               0
    ## 31          NA          NA          20               6               0
    ## 32          NA          NA          20               6               0
    ## 33          NA          NA          20               6               0
    ## 34          NA          NA          20               6               0
    ## 35          NA          NA          20               9               0
    ## 36          NA          NA          20               9               0
    ## 37          NA          NA          20               9               0
    ## 38          NA          NA          20               9               0
    ## 39          NA          NA          20               9               1
    ## 40          NA          NA          20               9               1
    ## 41          NA          NA          20               9               1
    ## 42          NA          NA          20               9               1
    ## 43          NA          NA          20               9               1
    ## 44          NA          NA          20               8               0
    ## 45          NA          NA          20               8               0
    ## 46          NA          NA          20               8               0
    ## 47          NA          NA          20               8               0
    ## 48        28.5          NA          20               8               0
    ## 49        28.5          NA          20               8               0
    ## 50        28.5          NA          20               8               0
    ## 51        28.5          NA          20               8               0
    ## 52        28.0          NA          19               9               0
    ## 53        28.0          NA          19               9               0
    ## 54        28.0          NA          19               9               0
    ## 55        28.0          NA          19               9               0
    ## 56        28.0          NA          19               9               0
    ## 57          NA          NA          20               7               0
    ## 58          NA          NA          20               7               0
    ## 59          NA          NA          20               8               0
    ## 60          NA          NA          20               8               0
    ## 61          NA          NA          20               8               0
    ## 62          NA          NA          20               8               0
    ## 63          NA          NA          19               5               0
    ## 64          NA          NA          19               5               0
    ## 65          NA          NA          20               7               0
    ## 66          NA          NA          20               7               0
    ## 67          NA          NA          20               7               0
    ## 68          NA          NA          20               7               0
    ## 69          NA          NA          20               7               0
    ## 70          NA          NA          20               9               1
    ## 71          NA          NA          20               9               1
    ## 72          NA          NA          20               9               1
    ## 73          NA          NA          20               8               0
    ## 74          NA          NA          20               8               0
    ## 75          NA          NA          20               8               0
    ## 76          NA          NA          20               8               0
    ## 77        28.5          NA          20               8               0
    ## 78        28.5          NA          20               8               0
    ## 79        28.5          NA          20               8               0
    ## 80        28.5          NA          20               8               0
    ## 81        28.0          NA          19               9               0
    ## 82        28.0          NA          19               9               0
    ## 83        28.0          NA          19               9               0
    ## 84          NA          NA          20               7               0
    ## 85          NA          NA          20               7               0
    ## 86          NA          NA          20               7               0
    ## 87          NA          NA          20               7               0
    ## 88          NA          NA          20               8               0
    ## 89          NA          NA          20               8               0
    ## 90          NA          NA          20               8               0
    ## 91          NA          NA          20               8               0
    ## 92          NA          NA          19               5               0
    ## 93          NA          NA          19               5               0
    ## 94          NA          NA          20               7               0
    ## 95          NA          NA          20               7               0
    ## 96        24.3        40.8          20               8               0
    ## 97        24.3        40.8          20               8               0
    ## 98        24.3        40.8          20               8               0
    ## 99        22.6        42.4          20               9               0
    ## 100       22.6        42.4          20               9               0
    ## 101       22.6        42.4          20               9               0
    ## 102       22.6        42.4          20               9               0
    ## 103       22.6        42.4          20               9               0
    ## 104       22.6        42.4          20               9               0
    ## 105       22.6        42.4          20               9               0
    ## 106       22.2        38.5          20               8               0
    ## 107       22.2        38.5          20               8               0
    ## 108       23.8        43.8          20               9               0
    ## 109       23.8        43.8          20               9               0
    ## 110       23.8        43.8          20               9               0
    ## 111       23.8        43.8          20               9               0
    ## 112       23.8        43.8          20               9               0
    ## 113       22.6        43.3          20              11               0
    ## 114       22.6        43.3          20              11               0
    ## 115       23.8        42.7          20               9               0
    ## 116       23.8        42.7          20               9               0
    ## 117       23.8        42.7          20               9               0
    ## 118       23.8        42.7          20               9               0
    ## 119       25.5        44.6          20               3               2
    ## 120       24.3        40.8          20               8               0
    ## 121       24.3        40.8          20               8               0
    ## 122       24.3        40.8          20               8               0
    ## 123       24.3        40.8          20               8               0
    ## 124       24.3        40.8          20               8               0
    ## 125       22.6        42.4          20               9               0
    ## 126       22.2        38.5          20               8               0
    ## 127       22.2        38.5          20               8               0
    ## 128       22.2        38.5          20               8               0
    ## 129       22.2        38.5          20               8               0
    ## 130       23.8        43.8          20               9               0
    ## 131       23.8        43.8          20               9               0
    ## 132       23.8        43.8          20               9               0
    ## 133       23.8        43.8          20               9               0
    ## 134       22.6        43.3          20              11               0
    ## 135       22.6        43.3          20              11               0
    ## 136       22.6        43.3          20              11               0
    ## 137       22.6        43.3          20              11               0
    ## 138       22.6        43.3          20              11               0
    ## 139       23.8        42.7          20               9               0
    ## 140       23.8        42.7          20               9               0
    ## 141       23.8        42.7          20               9               0
    ## 142       23.8        42.7          20               9               0
    ## 143       23.8        42.7          20               9               0
    ## 144       25.5        44.6          20               3               2
    ## 145       25.5        44.6          20               3               2
    ## 146       17.0        33.4          19               8               0
    ## 147       17.0        33.4          19               8               0
    ## 148       17.0        33.4          19               8               0
    ## 149       21.4        42.1          19               9               1
    ## 150       21.4        42.1          19               9               1
    ## 151       21.4        42.1          19               9               1
    ## 152         NA          NA          19               6               0
    ## 153         NA          NA          19               6               0
    ## 154         NA          NA          19               8               0
    ## 155         NA          NA          19               8               0
    ## 156         NA          NA          19               8               0
    ## 157         NA          NA          19               8               0
    ## 158         NA          NA          19               8               0
    ## 159         NA          NA          19               8               0
    ## 160         NA          NA          20               7               0
    ## 161         NA          NA          20               7               0
    ## 162         NA          NA          20               7               0
    ## 163         NA          NA          20               7               0
    ## 164       28.0        45.9          20               5               0
    ## 165       28.0        45.9          20               5               0
    ## 166       23.5          NA          19               9               0
    ## 167       23.5          NA          19               9               0
    ## 168       23.5          NA          19               9               0
    ## 169       23.5          NA          19               9               0
    ## 170       22.6        37.0          19               5               0
    ## 171       22.6        37.0          19               5               0
    ## 172       22.6        37.0          19               5               0
    ## 173         NA          NA          20               9               0
    ## 174         NA          NA          20               9               0
    ## 175         NA          NA          20               9               0
    ## 176         NA          NA          20               9               0
    ## 177       21.7        37.8          20               5               0
    ## 178       24.4        42.9          19               7               1
    ## 179       19.5        35.9          19               7               2
    ## 180       19.5        35.9          19               7               2
    ## 181       19.5        35.9          19               7               2
    ## 182       17.0        33.4          19               8               0
    ## 183       17.0        33.4          19               8               0
    ## 184       21.4        42.1          19               9               1
    ## 185       21.4        42.1          19               9               1
    ## 186       21.4        42.1          19               9               1
    ## 187       21.4        42.1          19               9               1
    ## 188       21.4        42.1          19               9               1
    ## 189       21.4        42.1          19               9               1
    ## 190         NA          NA          19               6               0
    ## 191         NA          NA          19               6               0
    ## 192         NA          NA          19               6               0
    ## 193         NA          NA          19               6               0
    ## 194         NA          NA          19               8               0
    ## 195         NA          NA          19               8               0
    ## 196         NA          NA          20               7               0
    ## 197         NA          NA          20               7               0
    ## 198         NA          NA          20               7               0
    ## 199       28.0        45.9          20               5               0
    ## 200       28.0        45.9          20               5               0
    ## 201       28.0        45.9          20               5               0
    ## 202       23.5          NA          19               9               0
    ## 203       23.5          NA          19               9               0
    ## 204       23.5          NA          19               9               0
    ## 205         NA          NA          NA              NA              NA
    ## 206         NA          NA          NA              NA              NA
    ## 207         NA          NA          20               9               0
    ## 208         NA          NA          20               9               0
    ## 209         NA          NA          20               9               0
    ## 210         NA          NA          20               9               0
    ## 211         NA          NA          20               9               0
    ## 212       21.7        37.8          20               5               0
    ## 213       24.4        42.9          19               7               1
    ## 214       24.4        42.9          19               7               1
    ## 215       19.5        35.9          19               7               2
    ## 216       21.8        37.2          20               8               1
    ## 217       21.8        37.2          20               8               1
    ## 218       21.8        37.2          20               8               1
    ## 219       21.8        37.2          20               8               1
    ## 220       21.8        37.2          20               8               1
    ## 221       25.4        43.8          19               8               0
    ## 222       25.4        43.8          19               8               0
    ## 223       25.4        43.8          19               8               0
    ## 224       25.4        43.8          19               8               0
    ## 225       25.4        43.8          19               8               0
    ## 226       20.0        39.2          20               8               0
    ## 227       20.0        39.2          20               8               0
    ## 228       21.8        35.2          20               4               0
    ## 229       21.8        35.2          20               4               0
    ## 230       21.8        35.2          20               4               0
    ## 231       25.6        47.5          20               8               0
    ## 232       25.6        47.5          20               8               0
    ## 233       25.6        47.5          20               8               0
    ## 234       25.6        47.5          20               8               0
    ## 235       23.5        39.0          20               6               0
    ## 236       23.5        39.0          20               6               0
    ## 237       23.5        39.0          20               6               0
    ## 238       23.5        39.0          20               6               0
    ## 239       25.5        42.7          20               7               0
    ## 240       21.8        37.2          20               8               1
    ## 241       21.8        37.2          20               8               1
    ## 242       25.4        43.8          19               8               0
    ## 243       25.4        43.8          19               8               0
    ## 244       20.0        39.2          20               8               0
    ## 245       20.0        39.2          20               8               0
    ## 246       20.0        39.2          20               8               0
    ## 247       20.0        39.2          20               8               0
    ## 248       20.0        39.2          20               8               0
    ## 249       21.8        35.2          20               4               0
    ## 250       25.6        47.5          20               8               0
    ## 251       25.6        47.5          20               8               0
    ## 252       25.6        47.5          20               8               0
    ## 253       23.5        39.0          20               6               0
    ## 254       25.5        42.7          20               7               0
    ## 255       25.5        42.7          20               7               0
    ## 256       25.5        42.7          20               7               0
    ## 257       25.5        42.7          20               7               0
    ## 258       25.5        42.7          20               7               0
    ## 259       24.5        42.8          20               8               1
    ## 260       24.5        42.8          20               8               1
    ## 261       24.5        42.8          20               8               1
    ## 262       24.5        42.8          20               8               1
    ## 263       24.5        42.8          20               8               1
    ## 264       24.5        42.8          20               8               1
    ## 265         NA        41.1          20              11               0
    ## 266         NA        41.1          20              11               0
    ## 267         NA        41.1          20              11               0
    ## 268         NA          NA          19               8               0
    ## 269         NA          NA          NA              NA              NA
    ## 270         NA          NA          NA              NA              NA
    ## 271         NA          NA          NA              NA              NA
    ## 272       27.5        46.0          19               8               1
    ## 273       27.5        46.0          19               8               1
    ## 274       27.5        46.0          19               8               1
    ## 275       27.5        46.0          19               8               1
    ## 276       27.5        46.0          19               8               1
    ## 277       27.5        46.0          19               8               1
    ## 278       28.5        44.5          20               9               0
    ## 279       28.5        44.5          20               9               0
    ## 280       28.5        44.5          20               9               0
    ## 281       33.4        52.7          20               8               0
    ## 282       33.4        52.7          20               8               0
    ## 283       33.4        52.7          20               8               0
    ## 284       24.5        42.8          20               8               1
    ## 285       24.5        42.8          20               8               1
    ## 286         NA        41.1          20              11               0
    ## 287         NA        41.1          20              11               0
    ## 288         NA        41.1          20              11               0
    ## 289         NA        41.1          20              11               0
    ## 290         NA        41.1          20              11               0
    ## 291         NA        41.1          20              11               0
    ## 292         NA          NA          19               8               0
    ## 293         NA          NA          19               8               0
    ## 294         NA          NA          19               8               0
    ## 295         NA          NA          19               8               0
    ## 296         NA          NA          19               8               0
    ## 297         NA          NA          19               8               0
    ## 298         NA          NA          19               8               0
    ## 299         NA          NA          NA              NA              NA
    ## 300         NA          NA          NA              NA              NA
    ## 301         NA          NA          NA              NA              NA
    ## 302         NA          NA          NA              NA              NA
    ## 303       27.5        46.0          19               8               1
    ## 304       27.5        46.0          19               8               1
    ## 305       28.5        44.5          20               9               0
    ## 306       28.5        44.5          20               9               0
    ## 307       28.5        44.5          20               9               0
    ## 308       28.5        44.5          20               9               0
    ## 309       28.5        44.5          20               9               0
    ## 310       28.5        44.5          20               9               0
    ## 311       33.4        52.7          20               8               0
    ## 312       33.4        52.7          20               8               0
    ## 313       33.4        52.7          20               8               0
    ##     pups_survive wt_gain
    ## 1              3    15.0
    ## 2              3    15.0
    ## 3              7    15.0
    ## 4              7    15.0
    ## 5              5    15.4
    ## 6              5    15.4
    ## 7              4    15.6
    ## 8              6      NA
    ## 9              6      NA
    ## 10             4      NA
    ## 11             9      NA
    ## 12             9      NA
    ## 13             9      NA
    ## 14             9      NA
    ## 15             9      NA
    ## 16             3    15.0
    ## 17             7    15.0
    ## 18             7    15.0
    ## 19             7    15.0
    ## 20             7    15.0
    ## 21             7    15.0
    ## 22             5    15.4
    ## 23             5    15.4
    ## 24             5    15.4
    ## 25             4    15.6
    ## 26             4    15.6
    ## 27             4    15.6
    ## 28             6      NA
    ## 29             6      NA
    ## 30             6      NA
    ## 31             6      NA
    ## 32             4      NA
    ## 33             4      NA
    ## 34             4      NA
    ## 35             9      NA
    ## 36             9      NA
    ## 37             9      NA
    ## 38             9      NA
    ## 39             8      NA
    ## 40             8      NA
    ## 41             8      NA
    ## 42             8      NA
    ## 43             8      NA
    ## 44             8      NA
    ## 45             8      NA
    ## 46             8      NA
    ## 47             8      NA
    ## 48             8      NA
    ## 49             8      NA
    ## 50             8      NA
    ## 51             8      NA
    ## 52             8      NA
    ## 53             8      NA
    ## 54             8      NA
    ## 55             8      NA
    ## 56             8      NA
    ## 57             6      NA
    ## 58             6      NA
    ## 59             8      NA
    ## 60             8      NA
    ## 61             8      NA
    ## 62             8      NA
    ## 63             4      NA
    ## 64             4      NA
    ## 65             7      NA
    ## 66             7      NA
    ## 67             7      NA
    ## 68             7      NA
    ## 69             7      NA
    ## 70             8      NA
    ## 71             8      NA
    ## 72             8      NA
    ## 73             8      NA
    ## 74             8      NA
    ## 75             8      NA
    ## 76             8      NA
    ## 77             8      NA
    ## 78             8      NA
    ## 79             8      NA
    ## 80             8      NA
    ## 81             8      NA
    ## 82             8      NA
    ## 83             8      NA
    ## 84             6      NA
    ## 85             6      NA
    ## 86             6      NA
    ## 87             6      NA
    ## 88             8      NA
    ## 89             8      NA
    ## 90             8      NA
    ## 91             8      NA
    ## 92             4      NA
    ## 93             4      NA
    ## 94             7      NA
    ## 95             7      NA
    ## 96             8    16.5
    ## 97             8    16.5
    ## 98             8    16.5
    ## 99             8    19.8
    ## 100            8    19.8
    ## 101            8    19.8
    ## 102            8    19.8
    ## 103            8    19.8
    ## 104            8    19.8
    ## 105            8    19.8
    ## 106            6    16.3
    ## 107            6    16.3
    ## 108            9    20.0
    ## 109            9    20.0
    ## 110            9    20.0
    ## 111            9    20.0
    ## 112            9    20.0
    ## 113            7    20.7
    ## 114            7    20.7
    ## 115            9    18.9
    ## 116            9    18.9
    ## 117            9    18.9
    ## 118            9    18.9
    ## 119            3    19.1
    ## 120            8    16.5
    ## 121            8    16.5
    ## 122            8    16.5
    ## 123            8    16.5
    ## 124            8    16.5
    ## 125            8    19.8
    ## 126            6    16.3
    ## 127            6    16.3
    ## 128            6    16.3
    ## 129            6    16.3
    ## 130            9    20.0
    ## 131            9    20.0
    ## 132            9    20.0
    ## 133            9    20.0
    ## 134            7    20.7
    ## 135            7    20.7
    ## 136            7    20.7
    ## 137            7    20.7
    ## 138            7    20.7
    ## 139            9    18.9
    ## 140            9    18.9
    ## 141            9    18.9
    ## 142            9    18.9
    ## 143            9    18.9
    ## 144            3    19.1
    ## 145            3    19.1
    ## 146            5    16.4
    ## 147            5    16.4
    ## 148            5    16.4
    ## 149            9    20.7
    ## 150            9    20.7
    ## 151            9    20.7
    ## 152            6      NA
    ## 153            6      NA
    ## 154            8      NA
    ## 155            8      NA
    ## 156            8      NA
    ## 157            8      NA
    ## 158            8      NA
    ## 159            8      NA
    ## 160            7      NA
    ## 161            7      NA
    ## 162            7      NA
    ## 163            7      NA
    ## 164            5    17.9
    ## 165            5    17.9
    ## 166            7      NA
    ## 167            7      NA
    ## 168            7      NA
    ## 169            7      NA
    ## 170            5    14.4
    ## 171            5    14.4
    ## 172            5    14.4
    ## 173            9      NA
    ## 174            9      NA
    ## 175            9      NA
    ## 176            9      NA
    ## 177            2    16.1
    ## 178            3    18.5
    ## 179            4    16.4
    ## 180            4    16.4
    ## 181            4    16.4
    ## 182            5    16.4
    ## 183            5    16.4
    ## 184            9    20.7
    ## 185            9    20.7
    ## 186            9    20.7
    ## 187            9    20.7
    ## 188            9    20.7
    ## 189            9    20.7
    ## 190            6      NA
    ## 191            6      NA
    ## 192            6      NA
    ## 193            6      NA
    ## 194            8      NA
    ## 195            8      NA
    ## 196            7      NA
    ## 197            7      NA
    ## 198            7      NA
    ## 199            5    17.9
    ## 200            5    17.9
    ## 201            5    17.9
    ## 202            7      NA
    ## 203            7      NA
    ## 204            7      NA
    ## 205           NA      NA
    ## 206           NA      NA
    ## 207            9      NA
    ## 208            9      NA
    ## 209            9      NA
    ## 210            9      NA
    ## 211            9      NA
    ## 212            2    16.1
    ## 213            3    18.5
    ## 214            3    18.5
    ## 215            4    16.4
    ## 216            7    15.4
    ## 217            7    15.4
    ## 218            7    15.4
    ## 219            7    15.4
    ## 220            7    15.4
    ## 221            7    18.4
    ## 222            7    18.4
    ## 223            7    18.4
    ## 224            7    18.4
    ## 225            7    18.4
    ## 226            7    19.2
    ## 227            7    19.2
    ## 228            4    13.4
    ## 229            4    13.4
    ## 230            4    13.4
    ## 231            7    21.9
    ## 232            7    21.9
    ## 233            7    21.9
    ## 234            7    21.9
    ## 235            5    15.5
    ## 236            5    15.5
    ## 237            5    15.5
    ## 238            5    15.5
    ## 239            6    17.2
    ## 240            7    15.4
    ## 241            7    15.4
    ## 242            7    18.4
    ## 243            7    18.4
    ## 244            7    19.2
    ## 245            7    19.2
    ## 246            7    19.2
    ## 247            7    19.2
    ## 248            7    19.2
    ## 249            4    13.4
    ## 250            7    21.9
    ## 251            7    21.9
    ## 252            7    21.9
    ## 253            5    15.5
    ## 254            6    17.2
    ## 255            6    17.2
    ## 256            6    17.2
    ## 257            6    17.2
    ## 258            6    17.2
    ## 259            8    18.3
    ## 260            8    18.3
    ## 261            8    18.3
    ## 262            8    18.3
    ## 263            8    18.3
    ## 264            8    18.3
    ## 265            9      NA
    ## 266            9      NA
    ## 267            9      NA
    ## 268            8      NA
    ## 269           NA      NA
    ## 270           NA      NA
    ## 271           NA      NA
    ## 272            8    18.5
    ## 273            8    18.5
    ## 274            8    18.5
    ## 275            8    18.5
    ## 276            8    18.5
    ## 277            8    18.5
    ## 278            9    16.0
    ## 279            9    16.0
    ## 280            9    16.0
    ## 281            6    19.3
    ## 282            6    19.3
    ## 283            6    19.3
    ## 284            8    18.3
    ## 285            8    18.3
    ## 286            9      NA
    ## 287            9      NA
    ## 288            9      NA
    ## 289            9      NA
    ## 290            9      NA
    ## 291            9      NA
    ## 292            8      NA
    ## 293            8      NA
    ## 294            8      NA
    ## 295            8      NA
    ## 296            8      NA
    ## 297            8      NA
    ## 298            8      NA
    ## 299           NA      NA
    ## 300           NA      NA
    ## 301           NA      NA
    ## 302           NA      NA
    ## 303            8    18.5
    ## 304            8    18.5
    ## 305            9    16.0
    ## 306            9    16.0
    ## 307            9    16.0
    ## 308            9    16.0
    ## 309            9    16.0
    ## 310            9    16.0
    ## 311            6    19.3
    ## 312            6    19.3
    ## 313            6    19.3
