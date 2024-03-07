---
title: "Louisiana Mortality Analytic Sample"
author: "Rebecca Letsinger"
date: "Feb 22, 2024"
output: pdf_document
---

```{r}
la_mort$cancer_parish <- ifelse(la_mort$cntyrsd %in% c(5, 33, 47, 51, 71, 89, 93, 95, 121), 1, 0)
```

```{r}
table(la_mort$cancer_parish)
table(la_mort$cntyrsd[la_mort$cancer_parish == 1])
```

```{r}
la_mort$cancer39 <- ifelse(la_mort$ucr39 %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), 1, 0)
```

```{r}
table(la_mort$cancer39)
```

```{r}
la_mort$cancer39 <- ifelse(la_mort$ucr39 %in% c(5:15), 1, 0)
```

```{r}
table(la_mort$ucr39[la_mort$cancer113 == 1 & la_mort$cancer39 == 0])
```

```{r}
table(la_mort$ucod[la_mort$cancer113 == 1 & la_mort$cancer39 == 0])
```

```{r}
library(dplyr)
parish_count <- la_mort %>%
  group_by(cntyrsd, cancer_parish, year) %>%
  summarize(cancer39 = sum(cancer39, na.rm = TRUE))
```

```{r}
summary(parish_count$cancer39)
```

```{r}
library(readr)
```

```{r}
la_pop <- 
  read_csv("https://www.dropbox.com/scl/fi/650k1obpczky6bwa19ex6/la_county_pop.csv?rlkey=0aokd9m76q7mxwus97uslsx7g&dl=1")
```

```{r}
parish_count <- parish_count %>%
  rename(county = cntyrsd)
```

```{r} 
la_joined <- parish_count %>%
  inner_join(la_pop, by = c("county", "year"))
  
```{r}
la_joined_all <- subset(la_joined, agegrp == "all")
```

```{r}
la_joined_all$cancer_rate_total <- (la_joined_all$cancer39) / (la_joined_all$tot_pop)
```

```{r}
summary(la_joined_all$cancer_rate_total)
```

```{r}
la_joined_all$cancer_rate_total <- ((la_joined_all$cancer39) / (la_joined_all$tot_pop / 100000))
```

```{r}
parish_cancer_2019 <- subset(la_joined_all, year == 2019)
library(knitr)
kable(parish_cancer_2019[, c("county", "cancer_rate_total")])
```
