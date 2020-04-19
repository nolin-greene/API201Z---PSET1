Harvard Kennedy School - API 201z
================
Nolin Greene
April 18, 2020

-----

**Question \#1: Case Study - Pine Street Inn**

*Data and Package Loading (output masked for readability purposes)*

``` r
rm(list=ls())

options(scipen = 999)

library(readxl);library(dplyr);library(ggplot2);library(plotly)
library(tidyr);library(knitr);library(kableExtra);library(stringr)

d<-read_excel("Pine Street Inn Length of Stay Data - Solutions.xls", 
              sheet = 1, cell_cols(1:2))

colnames(d)<-c("n","los")
```

*1.1:* The mean length of stay at Pine Street Inn is **26 days**.

*1.2:* The median length of stay at Pine Street Inn is **3 days**.

*1.3:* The maximum length of stay at Pine Street Inn is **727 days** and
the minimum length of stay is **1 day**.

*1.4:* The 75th percentile length of stay at Pine Street Inn is **17
days.** The 95th and 99th percentiles are **65 days** and **138 days**
respectively

*1.5:* There are **171905 bednights** represented in the dataset.

*1.6:* There are **6556 guests** represented in the dataset.

*1.7*

    ## # A tibble: 1 x 2
    ##       n bednights
    ##   <int>     <dbl>
    ## 1  3322      4973

    ## # A tibble: 1 x 2
    ##       n bednights
    ##   <int>     <dbl>
    ## 1  1177      7328

    ## # A tibble: 1 x 2
    ##       n bednights
    ##   <int>     <dbl>
    ## 1  1048     21007

    ## # A tibble: 1 x 2
    ##       n bednights
    ##   <int>     <dbl>
    ## 1   721     53832

    ## # A tibble: 1 x 2
    ##       n bednights
    ##   <int>     <dbl>
    ## 1   288     84765

### Summary Statistics for PSI Length of Stay

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Number of <br>Guests

</th>

<th style="text-align:left;">

Number of <br>Bed Nights

</th>

<th style="text-align:left;">

Fraction <br>of Guests

</th>

<th style="text-align:left;">

Fraction of <br>Bed Nights

</th>

<th style="text-align:left;">

Avg Bed Nights <br> per Guest

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

3 Days or Less

</td>

<td style="text-align:left;">

721

</td>

<td style="text-align:left;">

4973

</td>

<td style="text-align:left;">

0.11

</td>

<td style="text-align:left;">

0.03

</td>

<td style="text-align:left;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

4 to 10 Days

</td>

<td style="text-align:left;">

1177

</td>

<td style="text-align:left;">

7328

</td>

<td style="text-align:left;">

0.18

</td>

<td style="text-align:left;">

0.04

</td>

<td style="text-align:left;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

11 to 35 Days

</td>

<td style="text-align:left;">

1048

</td>

<td style="text-align:left;">

21007

</td>

<td style="text-align:left;">

0.16

</td>

<td style="text-align:left;">

0.12

</td>

<td style="text-align:left;">

20

</td>

</tr>

<tr>

<td style="text-align:left;">

36 to 150 Days

</td>

<td style="text-align:left;">

721

</td>

<td style="text-align:left;">

53832

</td>

<td style="text-align:left;">

0.11

</td>

<td style="text-align:left;">

0.31

</td>

<td style="text-align:left;">

75

</td>

</tr>

<tr>

<td style="text-align:left;">

151 Days or More

</td>

<td style="text-align:left;">

288

</td>

<td style="text-align:left;">

84765

</td>

<td style="text-align:left;">

0.04

</td>

<td style="text-align:left;">

0.49

</td>

<td style="text-align:left;">

294

</td>

</tr>

<tr>

<td style="text-align:left;">

Total

</td>

<td style="text-align:left;">

3955

</td>

<td style="text-align:left;">

171905

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

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

p<-ggplot(g,aes(x=bin, y=percent, group = stat, color=stat))+
  geom_line()+
  theme(legend.title = element_blank()) +
  labs(x="Length of Stay", y="Percent of Total", title = "Total Clients and Bednights by Length of Stay")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))

ggplotly(p)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-3f92d40edd35535f05ff" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-3f92d40edd35535f05ff">{"x":{"data":[{"x":[1,2,3,4,5],"y":[0.0289287687967191,0.042628195805823,0.12220121578779,0.313149704778802,0.493092114830866],"text":["bin: 3 Days or Less<br />percent: 0.02892877<br />stat: Bed_Nights<br />stat: Bed_Nights","bin: 4 to 10 Days<br />percent: 0.04262820<br />stat: Bed_Nights<br />stat: Bed_Nights","bin: 11 to 35 Days<br />percent: 0.12220122<br />stat: Bed_Nights<br />stat: Bed_Nights","bin: 36 to 150 Days<br />percent: 0.31314970<br />stat: Bed_Nights<br />stat: Bed_Nights","bin: 151 Days or More<br />percent: 0.49309211<br />stat: Bed_Nights<br />stat: Bed_Nights"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Bed_Nights","legendgroup":"Bed_Nights","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5],"y":[0.506711409395973,0.179530201342282,0.159853569249542,0.109975594874924,0.0439292251372788],"text":["bin: 3 Days or Less<br />percent: 0.50671141<br />stat: Clients<br />stat: Clients","bin: 4 to 10 Days<br />percent: 0.17953020<br />stat: Clients<br />stat: Clients","bin: 11 to 35 Days<br />percent: 0.15985357<br />stat: Clients<br />stat: Clients","bin: 36 to 150 Days<br />percent: 0.10997559<br />stat: Clients<br />stat: Clients","bin: 151 Days or More<br />percent: 0.04392923<br />stat: Clients<br />stat: Clients"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"hoveron":"points","name":"Clients","legendgroup":"Clients","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":43.1050228310502},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Total Clients and Bednights by Length of Stay","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["3 Days<br />or Less","4 to 10<br />Days","11 to 35<br />Days","36 to<br />150 Days","151 Days<br />or More"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["3 Days<br />or Less","4 to 10<br />Days","11 to 35<br />Days","36 to<br />150 Days","151 Days<br />or More"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Length of Stay","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.00503963676675642,0.530600541425936],"tickmode":"array","ticktext":["0.1","0.2","0.3","0.4","0.5"],"tickvals":[0.1,0.2,0.3,0.4,0.5],"categoryorder":"array","categoryarray":["0.1","0.2","0.3","0.4","0.5"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Percent of Total","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":1},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"d25c78a7ca38":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"d25c78a7ca38","visdat":{"d25c78a7ca38":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

*1.9:*

``` r
ggplot(d, aes(x=los))+
  geom_histogram(colour="black", fill = "slategray", binwidth = 10)+
  labs(x="Length of Stay (days)", y="Number of Guests", title="Distribution of Guests by Length of Stay")
```

![](PSET1_Markdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

*1.9:* Simply by looking at the mean, one might infer that it is common
for a PSI guest to spend 3-4 weeks in shelter. However, upon calculating
additional statistics (median, IQR, historgram), we see that the
distribution of length of stay is heavily right skewed, with a small
number of guests having very long stays. This leads me to believe that
Pine Street faces a very severe Pareto Principle, with a small number of
guests occupying an extreme proportion of the shelter’s total bed stays.

-----

**Question \#2: State Spending Data**

*A.* The total direct expenditure was $3.147tr. The total spent on
Elementary and Secondary Education was $565bn, the total spent on Health
was $84bn and the total spent on Corrections was $72.6bn.

*B.*

``` r
temp <- tempfile()
download.file("http://www2.census.gov/govs/local/11statetypepu.zip",temp)
state_exp <- read.table(unz(temp, "11statetypepu.txt"))
colnames(state_exp)<-c("govtype","itemcode","amount", "cv", "yr")
unlink(temp)
```

``` r
table(state_exp$yr)
```

    ## 
    ##    11 
    ## 30594

``` r
state_exp<-subset(state_exp, select = -yr)
state_exp<-filter(state_exp, govtype == 1)
state_exp<-subset(state_exp, select = -govtype)
state_exp$amount<-state_exp$amount/1000
state_exp<-state_exp %>%
  mutate(cat = case_when(
      itemcode=="E32" | itemcode=="F32" | itemcode=="G32" ~ "Health",
      itemcode=="E12" | itemcode=="F12" | itemcode=="G12" ~ "Education",
      itemcode=="E04" | itemcode=="F04" | itemcode=="G04" |
      itemcode=="E05" | itemcode=="F05" | itemcode=="G05" ~ "Corrections"))
state_exp<-filter(state_exp, cat %in% c("Health", "Education", "Corrections"))
state_exp %>%
  group_by(cat)%>%
  summarize(sum = sum(amount))
```

    ## # A tibble: 3 x 2
    ##   cat             sum
    ##   <chr>         <dbl>
    ## 1 Corrections  73243.
    ## 2 Education   565284.
    ## 3 Health       82392.
