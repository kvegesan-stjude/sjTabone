
#' @import tidyr
#' @import dplyr
#' @import rstatix
#' @import gt
#' @export

#' @title sjTabone
#'
#' @description Create a summarized table one for a given data set of continuous and categorical variables.
#'
# ' Load the libraries
library(dplyr)
library(tidyr)
library(data.table)
library(rstatix)
library(gt)

#'@title
#'
#' Write the output of the sjtabone function to a word document.
#'
#' @param tab.one A dataframe output from the function sjTabone.
#' @param filename Filename to write the output. It automatically adds a docx extension

write_tabone<-function(tab.one,filename){

  tab.one%>%mutate(pval=as.double(pval))%>%
    mutate(pval=if_else(is.na(pval),99999,pval))%>%
    gt() %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = cat,
                             rows = level == 1)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = pval,
                             rows = pval <=0.05)
    ) %>%
    cols_label(cat = "Variable") %>%
    tab_style(
      style = cell_text(align = 'right'),
      locations = cells_body(columns = cat,
                             rows = level == 2)
    ) %>%
    cols_hide(columns = c(level, method)) %>%
    tab_style(
      style = cell_borders(
        sides = "all",
        color = "#000000",
        style = "solid",
        weight = px(1)
      ),
      locations = cells_body(columns = everything(),
                             rows = everything())
    )%>%
    tab_style(
      style = cell_borders(
        sides = "all",
        color = "#000000",
        style = "solid",
        weight = px(1)
      ),
      locations = cells_column_labels(columns = everything())
    )%>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    )%>%sub_values(columns = pval,values = 99999,replacement = '')%>%
    gtsave(filename = paste0(filename,'.docx',collapse=''))
}


#'@title
#'
#' A helper function to pivot confidence intervals for normal/non-normal variables.
my_quantile <- function(x, probs) {
  f <- tibble(x = quantile(x, probs, na.rm = TRUE), probs = probs)
  pivot_wider(f, names_from = probs, values_from = x)
}
#'@title
#'
#' Runs the Fisher exact test and returns a dataframe with p-values. This function is used by summarize_categorical().
#'
#' @param data A dataframe with data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The categorical column/variable that is being stratified.
#' @param idcol This is the column with unique identifiers for each row of the data. This is the column that will be used to generate the counts to run the test.

run_fisher<-function(data,strata,i,idcol){
  x<-data %>% group_by(!!as.name(strata),!!as.name(i)) %>%
    summarise(n = n_distinct(!!as.name(idcol)))%>%
    pivot_wider(names_from = !!as.name(strata),
                values_from = n)
  x[,2:ncol(x)]<-x[,2:ncol(x)]%>%mutate(across(everything(),~replace_na(.x,0)))
  x<-as.data.frame(x)
  x[,1] <- factor(x[,1],
                  exclude = NULL,
                  levels = c(levels(x[,1]), NA),
                  labels = c(levels(x[,1]), "None"))
  rownames(x)<-x[,1]
  x[,1]<-NULL
  f.test<-as.data.frame( fisher_test(x,simulate.p.value = TRUE))
  return(f.test)
}

#'@title
#'
#' This function runs the Kruskal-Wallis test on non-normal data. This function is used by summarize_nonnormal.
#'
#' @param data Dataframe of your data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The non-normal column/variable that is being stratified.
run_kwtest<-function(data,strata,i){
  formla<-as.formula(paste0(i,"~",strata))
  kw.test<-data%>%kruskal_test(formula = formla)
  kw.test<-as.data.frame(kw.test)
  return(kw.test)
}
#'@title
#'
#' This functions runs anova on normally distributed columns/variables. It's used by summarize_normal.
#'
#' @param data Dataframe of your data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The normal column/variable that is being stratified.
run_anova<-function(data,strata,i){
  formla<-as.formula(paste0(i,"~",strata))
  av.test<-data%>%anova_test(formula = formla)
  av.test<-as.data.frame(av.test)
  return(av.test)
}

#'@title Summarize Categorical variables.
#'@description
#' Function to summarize categorical variables. It's used by sjtabone.
#' This function can also be used as a stand alone function.
#' @param data Dataframe of your data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The categorical column/variable that is being stratified.
#' @param idcol This is the column with unique identifiers for each row of the data. This is the column that will be used to generate the counts to run the test.
#' @param grps If your stratifying column has more than 2 groups, and you only want to summarize a subset of them, pass those values to this function.
summarize_categorical <- function(data, strata, i,idcol,grps=NULL) {
  method<-"categorical"
  temp <- data %>% group_by(!!as.name(strata),!!as.name(i)) %>%
    summarise(n = n_distinct(!!as.name(idcol))) %>%
    mutate(freq = n * 100 / sum(n)) %>%
    mutate(val = paste0(n, '[', round(freq, 1) , ']')) %>%
    # mutate(var=i)%>%
    rename(cat = !!as.name(i)) %>%
    mutate(cat = as.character(cat)) %>%
    select(c(!!as.name(strata), cat, val)) %>%
    pivot_wider(names_from = !!as.name(strata),
                values_from = val)#%>%      mutate(cat=paste0("              ",cat))
  temp$level <- 2
  temp$pval<-""
  temp$method <- ""
  #Fisher test
  if(length(unique(data[[i]]))<2){
    f.test<-c(p="N/A")
  }else{
    if(is.null(grps))
    {
      f.test<-run_fisher(data,strata,i,idcol)
    }
    else{
      fltr.data<-data%>%filter(!!as.name(strata) %in% grps)
      f.test<-run_fisher(fltr.data,strata,i,idcol)
    }
  }
  #Create a table with categories, n and p-val
  temp <-
    rbind(c(paste(i, "(n(%))"), rep("", ncol(temp) - 4), 1,f.test[['p']], method), temp)
  return(temp)

}
#'@title Function to summarize non-normal variables
#'@description
#'This function is used by sjtabone. It can also be used as a stand alone function.
#' @param data Dataframe of your data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The non-normal column/variable that is being stratified.
#' @param grps If your stratifying column has more than 2 groups, and you only want to summarize a subset of them, pass those values to this function.

summarize_nonnormal <- function(data, strata, i,grps=NULL) {
  method <- 'nonnormal'
  temp <- data %>% group_by(!!as.name(strata)) %>%
    summarise(n = median(!!as.name(i), na.rm = TRUE),
              iqr = my_quantile(!!as.name(i), probs = c(0.25, 0.75))) %>%
    mutate(val = paste0(
      round(n, 2),
      '[',
      round(iqr$`0.25`, 2),
      '-',
      round(iqr$`0.75`, 2),
      ']'
    )) %>%
    # rename(cat = !!as.name(i)) %>%
    # mutate(cat = as.character(cat)) %>%
    select(c(!!as.name(strata), val)) %>%
    pivot_wider(names_from = !!as.name(strata),
                values_from = val)
  temp <- cbind(cat = i, temp)
  temp$level <- 1

  if(is.null(grps))
  {
    kw.test<-run_kwtest(data,strata,i)
  }
  else{

    fltr.data<-data%>%filter(!!as.name(strata) %in% grps)
    kw.test<-run_kwtest(fltr.data,strata,i)
  }
  temp$pval<-kw.test$p

  temp$method <- method


  return(temp)
}

#'@title Function to summarize normal variables.
#'@description
#'This function is used by sjtabone. It can also be used as a stand alone function.
#' @param data Dataframe of your data.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param i The normal column/variable that is being stratified.
#' @param grps If your stratifying column has more than 2 groups, and you only want to summarize a subset of them, pass those values to this function.
summarize_normal <- function(data, strata, i,grps=NULL) {
  method <- "normal"
  #
  temp <- data %>% group_by(!!as.name(strata)) %>%
    summarise(
      n = n(),
      mean = mean(!!as.name(i), na.rm = TRUE),
      sd = sd(!!as.name(i), na.rm = TRUE)
    ) %>%
    mutate(
      se = sd / sqrt(n),
      lowerci = mean - qt(1 - (0.05 / 2), n - 1) * se,
      upperci = mean + qt(1 - (0.05 / 2), n - 1) * se
    ) %>%
    mutate(val = paste0(
      round(mean, 2),
      '[',
      round(lowerci, 2),
      '-',
      round(upperci, 2),
      ']'
    )) %>%
    select(c(!!as.name(strata), val)) %>%
    pivot_wider(names_from = !!as.name(strata),
                values_from = val)
  temp <- cbind(cat = i, temp)
  temp$level <- 1
  if(is.null(grps))
  {
    av.test<-run_anova(data,strata,i)
  }
  else{

    fltr.data<-data%>%filter(!!as.name(strata) %in% grps)
    av.test<-run_anova(fltr.data,strata,i)
  }
  temp$pval<-av.test$p
  temp$method <- method
  return(temp)
}






#' @title Summarize your dataset and and compare between stratifying groups.
#' @description
#'
#' Function to summarize your dataset. For every variable in the vector myvars the function checks whether it's categorical, or continuous.
#' If it's continuous it tests the normality of the variable to summarize appropriately.
#' It returns a dataframe with counts and percentages for the categorical variable. The normally distributed variables are summarized by mean and standard deviation.
#' Non-normal variables are summarized median and inter-quartile ranges.
#' Statistical comparisons are made between the different groups in the stratification column using the aprropriate statistical tests.
#' Categorical variables are compared using the Fisher exact test.
#' Normal variables are compared using ANOVA.
#' Non-normal variables are compared using the Kruskal-Wallis test.
#' @param data Dataframe of your data.
#' @param myvars Vector of column names form your dataset that need to be summarized. Categorical variables wil need to be cast as factors before passing the dataframe to the function.
#' @param idcol This is the column with unique identifiers for each row of the data. This is the column that will be used to generate the counts to run the test.
#' @param strata The column to use for stratification of the data into cases/controls.
#' @param grps If your stratifying column has more than 2 groups, and you only want to summarize a subset of them, pass those values to this function.

sjTabone<-function(data,myvars,idcol,strata,grps=NULL){

  first <- TRUE
  for (i in myvars) {
    print(i)
    if(i %in% colnames(data)){
      if ((i != idcol) & (i != strata) & (is.factor(data[[i]])))
      {
        method <- "categorical"
        temp <- summarize_categorical(data = data,
                                      strata = strata,
                                      i = i,grps=grps,idcol = idcol)

        if (first) {
          fulltable <- temp
          first <- FALSE
        }
        else{
          fulltable <- rbind(fulltable, temp)
        }
      }
      else if ((i != idcol) & (i != strata)) {
        tst <- shapiro.test(as.numeric(data[[i]]))
        if (tst$p.value < 0.05) {
          method <- 'nonnormal'
          temp <- summarize_nonnormal(data = data,
                                      strata = strata,
                                      i = i,grps=grps)

          if (first) {
            fulltable <- temp
            first <- FALSE
          }
          else{
            fulltable <- rbind(fulltable, temp)
          }
          # print(temp)
        }
        #Normally distributed
        else{
          method <- "normal"
          #
          temp <- summarize_normal(data=data,strata=strata,i=i,grps=grps)


          if (first) {
            fulltable <- temp
            first <- FALSE
          }
          else{
            fulltable <- rbind(fulltable, temp)
          }

        }


      }
    }
  }
  temp <- data %>% group_by(!!as.name(strata)) %>%
    summarise(val = n_distinct(!!as.name(idcol))) %>%
    pivot_wider(names_from = !!as.name(strata),
                values_from = val)
  temp <- cbind(cat = "Count",
                temp,
                level = 1,
                pval='',
                method = "")
  fulltable <- rbind(temp, fulltable)
  return(fulltable)
}


