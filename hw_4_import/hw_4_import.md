hw\_4\_import
================
Jay Lee

### 1. Houston Traffic

This data set comes from a survey done in Harris County, TX, starting when the city of Houston was recovering from the recession of the 1980s. I focused on issues dealing with traffic. My motivating question: do opinions on the state of Houston traffic change as you move farther from the city center? I expect these to become less important as we move outwards.

``` r
houston <- read.dta("C:/Users/Jay/Desktop/Data Science 241/jay_lee/hw_4_import/Houston/DS0001/20428-0001-Data.dta") %>%
  select(YEAR, # year of survey
         BIGPROB1, # biggest problem facing Houston, answers include Traffic
         LIVEZIPS) # where in the city the respondent lives
```

``` r
ByYear <- houston %>%
  filter(!is.na(LIVEZIPS)) %>%
  group_by(YEAR, LIVEZIPS) %>%
  dplyr::summarize(tmean = mean(BIGPROB1 == "Traffic", na.rm = T)) %>% 
  ungroup()
# proportion who see traffic as biggest issue

ggplot(ByYear, aes(x = YEAR,
                   y = tmean,
                   color = LIVEZIPS)) +
  geom_point() +
  geom_line(aes(group = LIVEZIPS)) +
  labs(title = "Houston Traffic Opinions by Region",
       x = "Year",
       y = "Proportion Listing Traffic as #1 Issue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](hw_4_import_files/figure-markdown_github/tidy%20plot%201-1.png)

No clear pattern emerges in ranking. If anything, among the three locations the `Inside Loop 610` group (closer to downtown) ranks traffic the lowest in priority, against my expectations. After considering other values for `BIGPROB1`, however, this makes more sense. Namely, I expect the Crime and Schools/Children categories should have higher proportions there than farther from the center of the city.

### 2. Determining Recent "Swing States"

This data set comes from a Wikipedia page detailing over time whether each state voted for a Democratic or Republican presidential candidate. My interest in this question comes from a graph presented in Abramson, Aldrich, Gomez, and Rohde's *Change and Continuity in the 2012 Elections*. The graph I present is an updated version of their figure 3-3. My motivating question: how consistently have different states voted in the last 5 presidential elections?

**Edit**: The chart I found was too much trouble to deal with so instead I pulled a chart from the page that shows overall percentage voting for each party.

``` r
state <- "https://en.wikipedia.org/wiki/List_of_United_States_presidential_election_results_by_state" %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[6]') %>%
  html_table(header = TRUE)
state <- state[[1]]
```

``` r
state2 <- state %>%
  select(State,
         `Votes since 1856`,
         `D votes since 1856`) %>%
  transmute(region = tolower(State), # choroplethr is really particular about format
            value = `D votes since 1856` / `Votes since 1856`)
state2[9,1] <- "district of columbia"
# Rename "d.c" to "district of columbia" for choroplethr to manage
state_choropleth(state2,
                 title   = "State History of Voting Democratic (Since 1856)",
                 legend  = "Percentage Democratic")
```

![](hw_4_import_files/figure-markdown_github/tidy%20plot%202-1.png)

I realize this isn't as relevant after the realignment of the South, but it's still interesting. Since the Civil War, the South (currently a Republican stronghold) has voted more Democratic than other parts of the country.

### 3. Campaign Finance Data

This data set is a financial summary of Political Action Committee (PAC) expenditures, from the Federal Election Commission. My motivating question: which types of campaign groups have the most expenditures?

``` r
names <- as.list(read_csv("C:/Users/Jay/Desktop/Data Science 241/FEC/webk_header_file.csv", col_names = F))

FEC <- read.delim("C:/Users/Jay/Desktop/Data Science 241/FEC/webk16.txt", header = FALSE, sep = "|")
colnames(FEC) <- names
```

``` r
FEC <- FEC %>%
  select(CMTE_NM,
         CMTE_TP,
         TTL_RECEIPTS) %>%
  mutate(CMTE_TP = fct_recode(CMTE_TP,
                              "Communication Cost" = "C",
                              "Delegate Committee" = "D",
                              "Electioneering Communication" = "E",
                              "House" = "H",
                              "Independent Expenditor" = "I",
                              "PAC - Nonqualified" = "N",
                              "Super PAC" = "O",
                              "Presidential" = "P",
                              "PAC - Qualified" = "Q",
                              "Senate" = "S",
                              "Single Candidate Independent" = "U",
                              "PAC w/ NCA - Qualified" = "V",
                              "PAC w/ NCA - Nonqualified" = "W",
                              "Party - Nonqualified" = "X",
                              "Party - Qualified" = "Y")) %>%
  group_by(CMTE_TP) %>%
  dplyr::summarize(mean = mean(TTL_RECEIPTS, na.rm = T),
            median = median(TTL_RECEIPTS, na.rm = T)) %>%
  gather(key = stat, value = val, mean, median)
ggplot(FEC, aes(x = CMTE_TP, y = val, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(x = "Committee Type",
       y = "mean - median",
       title = "2015-2016 PAC Expenditures") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw_4_import_files/figure-markdown_github/tidy%20plot%203-1.png)
