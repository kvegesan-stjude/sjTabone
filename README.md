# sjTabone

We developed this package to help create a summary table one for publications. There are other excellent options like [DescTools](https://cran.r-project.org/web/packages/DescTools/index.html) and [tableone](https://github.com/kaz-yos/tableone) that can help you do create the same summaries. We created this package so we could customize the methods and display options.

When using this package, categorical variables need to be explicitly converted to factors. Comparisons between groups are done using the following methods:

-   Categorical: [Fisher's exact test](https://en.wikipedia.org/wiki/Fisher%27s_exact_test)

-   Normally Distributed Variables: [ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance)

-   Non-Normal Variables: [Kruskal--Wallis one-way analysis of variance](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_one-way_analysis_of_variance)

[Shapiro-Wilk test](https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test) is used to test the normality of variables.

### Installation

```{r}

devtools::install_github("https://github.com/kvegesan-stjude/sjTabone")


```

### **Usage**

Let's create some test data with an outcome variable, one categorical variable, one normally distributed variable, and one non-normal variable:

```{r}
library(sjTabone)
data <-
  as.data.frame(list(
    subject.id=seq(1:100),
    outcome = sample(
      c('case', 'control'),
      size = 100,
      replace = TRUE,
      prob = c(0.3, 0.7)
    ),
    sex = sample(
      c('Male', 'Female'),
      size = 100,
      replace = TRUE,
      prob = c(0.5, 0.5)
    ),
    age = log(rlnorm(
      100, meanlog = 28, sdlog = 4
    )),
    bmi = c(log(rlnorm(
      70, meanlog = 20, sdlog = 2
    )), log(rlnorm(
      30, meanlog = 32, sdlog = 2
    )))
  ))

```

Cast the categorical variable as as factor. This step is to avoid the cases where numeric factors are treated as continuous variables.

```{r}
library(dplyr)
catvars<-c('sex')
data<-data%>%mutate_at(catvars, factor)
```

We can now call sjTabone on this dataset and save the table to a document called 'Summary1':

```{r}
sumry<-sjTabone(data,myvars = colnames(data),idcol = 'subject.id',strata = 'outcome')

write_tabone(sumry,'Summary1')
```

This is an example of the output.

![Summary](Summary1.png)
