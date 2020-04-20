Harvard Kennedy School - API 201z
================
Nolin Greene
April 18, 2020

-----

### Preparations:

#### Operating System & Software Used:

  - MacBook Pro (Retina, 13-inch, Late 2013)  
  - macOS Catalina 10.15.4  
  - RStudio Version 1.2.5003

#### Options & Packages Loaded:

``` r
rm(list=ls())

options(scipen = 999)

library(readxl);library(dplyr);library(ggplot2);library(plotly)
library(tidyr);library(knitr);library(kableExtra);library(stringr)
```

-----

## Question \#1: Case Study - Pine Street Inn

#### Load Data:

``` r
d<-read_excel("Pine Street Inn Length of Stay Data - Solutions.xls", 
              sheet = 1, cell_cols(1:2))

colnames(d)<-c("n","los")
```

#### Analysis:

*1.1:* The mean length of stay at Pine Street Inn is **\(26\) days**.

*1.2:* The median length of stay at Pine Street Inn is **\(3\) days**.

*1.3:* The maximum length of stay at Pine Street Inn is **\(727\) days**
and the minimum length of stay is **\(1\) day**.

*1.4:* The 75th percentile length of stay at Pine Street Inn is **\(17\)
days.** The 95th and 99th percentiles are **\(65\) days** and **\(138\)
days** respectively

*1.5:* There are **\(171,905\) bednights** represented in the dataset.

*1.6:* There are **\(6556\) guests** represented in the
dataset.

*1.7*

### Summary Statistics for PSI Length of Stay

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Guest and Bed Night Statistics by Length of Stay Category

</caption>

<thead>

<tr>

<th style="text-align:right;">

Number of <br>Guests

</th>

<th style="text-align:right;">

Number of <br>Bed Nights

</th>

<th style="text-align:right;">

Fraction <br>of Guests

</th>

<th style="text-align:right;">

Fraction of <br>Bed Nights

</th>

<th style="text-align:right;">

Avg Bed Nights <br> per Guest

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

721

</td>

<td style="text-align:right;">

4,973

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

0.03

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

1,177

</td>

<td style="text-align:right;">

7,328

</td>

<td style="text-align:right;">

0.18

</td>

<td style="text-align:right;">

0.04

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:right;">

1,048

</td>

<td style="text-align:right;">

21,007

</td>

<td style="text-align:right;">

0.16

</td>

<td style="text-align:right;">

0.12

</td>

<td style="text-align:right;">

20

</td>

</tr>

<tr>

<td style="text-align:right;">

721

</td>

<td style="text-align:right;">

53,832

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

75

</td>

</tr>

<tr>

<td style="text-align:right;">

288

</td>

<td style="text-align:right;">

84,765

</td>

<td style="text-align:right;">

0.04

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

294

</td>

</tr>

<tr>

<td style="text-align:right;">

3,955

</td>

<td style="text-align:right;">

171,905

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

*1.8:*

``` r
d<-d %>%
  mutate(bin = case_when(
      los<4 ~ "3 Days or Less",
      los>3 & los<11 ~ "4 to 10 Days",
      los>10 & los <36 ~ "11 to 35 Days",
      los>35 & los<151 ~"36 to 150 Days",
      los>150 ~ "151 Days or More"),
    bin = factor(bin, levels = c("3 Days or Less","4 to 10 Days","11 to 35 Days",
                 "36 to 150 Days", "151 Days or More")))

count<-group_by(d, bin)%>%
  summarise(n = n(), los=sum(los))

count<-mutate(count,
       Clients = count$n/sum(count$n),
       Bed_Nights = count$los/sum(count$los))

count<-count[c(1,4,5)]

g<-gather(count, stat, percent, -bin)

ggplot(g,aes(x=bin, y=percent, group = stat, color=stat))+
  geom_line()+
  theme(legend.title = element_blank()) +
  labs(x="Length of Stay", y="Percent of Total", 
       title = "Total Clients and Bednights by Length of Stay")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
```

![](PSET1_Markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

*1.9:*

``` r
ggplot(d, aes(x=los))+
  geom_histogram(colour="black", fill = "slategray", binwidth = 10)+
  labs(x="Length of Stay (days)", y="Number of Guests", 
       title="Distribution of Guests by Length of Stay")
```

![](PSET1_Markdown_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

*1.9:* Simply by looking at the mean, one might infer that it is common
for a PSI guest to spend 3-4 weeks in shelter. However, upon calculating
additional statistics (median, IQR, historgram), we see that the
distribution of length of stay is heavily right skewed, with a small
number of guests having very long stays. This leads me to believe that
Pine Street faces a very severe Pareto Principle, with a small number of
guests occupying an extreme proportion of the shelter’s total bed stays.

-----

## Question \#2: State Spending Data

*A.* The total direct expenditure was $3.147tr. The total spent on
Elementary and Secondary Education was $565bn, the total spent on Health
was $84bn and the total spent on Corrections was $72.6bn.

*B.* The data are organized into columns separated by spaces. There
appears to be five columns, with each row separated on a different line.
The state code is contained in the row’s first two characters (Delaware
= “08”) and the item code is contained in characters with position 5-7
(Construction for Primary & Secondary Education = “F12”).

*C.* This chunk downloads and extracts our zip file, and creates a data
frame from it.

``` r
temp <- tempfile()
download.file("http://www2.census.gov/govs/local/11statetypepu.zip",temp)
state_exp <- read.table(unz(temp, "11statetypepu.txt"))
unlink(temp)
```

This chunk cleans and tidys our data.

``` r
colnames(state_exp)<-c("govtype","itemcode","amount", "cv", "yr")
state_exp$govtype<-formatC(state_exp$govtype, width = 3, format = "d", flag = "0")
state_exp<-transform(state_exp, state = substr(govtype,1,2), 
                     gov_level = substr (govtype, 3,3))
state_exp<-filter(state_exp, gov_level == 1)
state_exp<-select(state_exp, -govtype, -yr, -gov_level)
state_exp$amount<-state_exp$amount/1000
```

This chunk creates three clear categories of spend and groups the rows
by that spend and each state.

``` r
state_exp<-state_exp %>%
  mutate(cat = case_when(
      itemcode=="E32" | itemcode=="F32" | itemcode=="G32" ~ "Health",
      itemcode=="E12" | itemcode=="F12" | itemcode=="G12" ~ "Education",
      itemcode=="E04" | itemcode=="F04" | itemcode=="G04" |
      itemcode=="E05" | itemcode=="F05" | itemcode=="G05" ~ "Corrections"))
state_exp<-filter(state_exp, cat %in% c("Health", "Education", "Corrections"))
state_exp <- state_exp %>%
  group_by(state,cat)%>%
  summarize(sum = sum(amount))
```

The above code found rows whose item codes are related to Health,
Education or Corrections and summed all expenditures in the dataset by
those groupings. The result is a simple data frame displaying how much
was expended across the country for those three categories (in
millions).

*D.* The categories of E32, F32 and G32 represent “Current Operations”,
Construction" and “Other Capital Outlays” for “other” health
expenditures not otherwise captured in other item codes.

*E.* One reason we need to look at combined state and local expenditures
is because different states have different frameworks for allocating
governmental responsibilities within policy areas. For example, some
states might take on the majority of the financial and administrative
burden for maintaining corrections, while other states might cede that
authority to the local level.

#### Adding on State Names

``` r
statenames<-read.csv("statenames.csv", 
                     colClasses = c(state = "character", 
                                    names = "character", 
                                    pop = "numeric"))
state_exp<-left_join(state_exp, statenames, by = "state")
spendclean<-filter(state_exp,state != "00")[,2:5]
```

*F.* The data are now represented in millions. We converted them from
thousands to millions by dividing the original value by 1000.

*G*

``` r
spendclean<-spendclean %>%
  mutate(per_capita = sum*1000000/pop)
```

*H*

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Summary Statistics for State Per Capita Spending

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Mean

</th>

<th style="text-align:right;">

StdDev

</th>

<th style="text-align:right;">

Median

</th>

<th style="text-align:right;">

IQR

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Health

</td>

<td style="text-align:right;">

265.1

</td>

<td style="text-align:right;">

113.6

</td>

<td style="text-align:right;">

235.9

</td>

<td style="text-align:right;">

127.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Education

</td>

<td style="text-align:right;">

1,870.6

</td>

<td style="text-align:right;">

491.4

</td>

<td style="text-align:right;">

1,803.0

</td>

<td style="text-align:right;">

361.6

</td>

</tr>

<tr>

<td style="text-align:left;">

Corrections

</td>

<td style="text-align:right;">

221.1

</td>

<td style="text-align:right;">

68.9

</td>

<td style="text-align:right;">

200.4

</td>

<td style="text-align:right;">

83.5

</td>

</tr>

</tbody>

</table>

*I (i)* The formula for the variance of health spending can be denoted
by the following:

\[
\Large
\begin{aligned}
\sigma^{2} = \frac{\sum\limits_{i=1}^{n} \left(x_{i} - \overline{x}\right)^{2}} {n-1}
\end{aligned}
\]

*I (ii)* The variance is the sum of the squared differences of each
observation and the mean, divided by the number of observations minus 1.

*I (iii)*

``` r
x1<-mean(pc_health)
x2<-pc_health-x1
x3<-x2^2

sum(x3)/length(x3)-1
```

    ## [1] 12653.7

``` r
sqrt(sum(x3)/length(x3)-1)
```

    ## [1] 112.4887

*J*
![](PSET1_Markdown_files/figure-gfm/make%20first%20scatterplot%20of%20sum%20spending-1.png)<!-- -->
The plot shows a strong postiive correlation between total education and
health spending. This is to be expeceted because the larger the
population of the state, the more they are likely to spend on both
education and
health.

![](PSET1_Markdown_files/figure-gfm/make%20second%20scatterplot%20of%20per%20capita%20spending-1.png)<!-- -->
TThere is no immediate relationship between per capita Health spending
and per capital Education spending (though a few outliers may skew the
relationship slightly positive). The plot shows a strong postiive
correlation between total education and health spending. This is to be
expected though since different states may have different priorities
with regards to health and education and may fund them at different
levels relative to the size of their populations.

*K*
![](PSET1_Markdown_files/figure-gfm/histogram%20of%20pc_corrections-1.png)<!-- -->

*L*

``` r
spendclean_alaska<-mutate(spendclean_wide, per_capita_Corrections = ifelse(
  names == "Alaska", 10000, per_capita_Corrections))

sum_stats_alaska<-data.frame(Mean = c(mean(spendclean_alaska$per_capita_Corrections)),
           StdDev = c(sd(spendclean_alaska$per_capita_Corrections)),
           Median = c(median(spendclean_alaska$per_capita_Corrections)),
           IQR = c(IQR(spendclean_alaska$per_capita_Corrections)),
           row.names ="Corrections")

kable(sum_stats_alaska,
      row.names = T,
      digits = 1,
      caption = "Summary Statistics for Per Capita Corrections Spending 
      - Inflated Alaska Example",
      escape = F)%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Summary Statistics for Per Capita Corrections Spending - Inflated Alaska
Example

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Mean

</th>

<th style="text-align:right;">

StdDev

</th>

<th style="text-align:right;">

Median

</th>

<th style="text-align:right;">

IQR

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Corrections

</td>

<td style="text-align:right;">

409.3

</td>

<td style="text-align:right;">

1371.3

</td>

<td style="text-align:right;">

200.4

</td>

<td style="text-align:right;">

83.5

</td>

</tr>

</tbody>

</table>

The median and the IQR remain unchanged from the original per capita
corrections summary, but the mean and the standard deviation have
increased dramatically due to Alaska now being a considerable outlier in
the dataset.
