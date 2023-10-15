#!/usr/bin/Rscript 
# Copyright (C) Abhishek Sheetal
# This file is part of HILDA methods project
#
# HILDA methods is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HILDA methods is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HILDA methods If not, see <http://www.gnu.org/licenses/>.
#
# This file reads the HILDA datasets, performs EDA and creates a table 
# that can be used by XGBOOST model building
#
suppressPackageStartupMessages({
  library(mice)
  library(tidyr)
  library(haven)
  library(data.table)
  library(xlsx)
  library(Hmisc)
  library(readxl)
  library(janitor)
  library(caret)
  library(corrplot)
  library(splitstackshape)
  library(dplyr)
  library(ggcorrplot)
  library(caTools)
  library(missRanger)
  library(filenamer)
  library(dlookr)
  library(ggplot2)
  library(gridExtra)
  library(ggthemr)
  library(ggthemes)
  library(ggpubr)
  library(gridExtra)
  library(naniar)
  library(MissMech)
  library(modi)
  library(performance)
  library(kableExtra)
  library(psych)
  library(mctest)
  library(lmtest)
  library(nortest)
  library(mltools)
})

corr_image <- "/research/dataset/HILDA/HILDA_methods/corr_image.png"
big5_density_image <- "/research/dataset/HILDA/HILDA_methods/big5_density_image.png"
big5_age_image <- "/research/dataset/HILDA/HILDA_methods/big5_age_image.png"
missingness_image <- "/research/dataset/HILDA/HILDA_methods/missingness_image.png"
variability_image <- "/research/dataset/HILDA/HILDA_methods/variability_image.png"
questions_file <- "/research/dataset/HILDA/HILDA_methods/chan_questions.tex"
cronbachalpha_file <- "/research/dataset/HILDA/HILDA_methods/cronbachalpha.tex"
omcdiag_file <- "/research/dataset/HILDA/HILDA_methods/omcdiag.tex"
bptest_file <- "/research/dataset/HILDA/HILDA_methods/bptest.tex"
hilda_file <- "/research/dataset/HILDA/HILDA_methods/hilda.tex"
participation_image <- "/research/dataset/HILDA/HILDA_methods/participation_image.png"

big5 <- c("extrav", "agree", "consc", "neurotic", "open")

my_shuffle <- function (df=NULL) {
  rows <- sample(nrow(df))
  return(df[rows, ])
}

options(filenamer.timestamp=1)
SAVE_FILE <- filename("all_data",
                      path="/research/dataset/HILDA/HILDA_methods",
                      tag=NULL,
                      ext="rds",
                      subdir=FALSE) %>%
  as.character() %>%
  print()

if (TRUE) {
  my_read_dta <- function(fname) {
    tdf <- read_dta(fname) %>%
      zap_labels()
    return(tdf)
  }
  #personality are in e i m
  {
    df_a <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_a180c.dta")
    df_b <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_b180c.dta")
    df_c <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_c180c.dta")
    df_d <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_d180c.dta")
    df_e <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_e180c.dta")
    df_f <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_f180c.dta")
    df_g <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_g180c.dta")
    df_h <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_h180c.dta")
    df_i <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_i180c.dta")
    df_j <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_j180c.dta")
    df_k <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_k180c.dta")
    df_l <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_l180c.dta")
    df_m <- my_read_dta("/research/dataset/HILDA/2. STATA 180c (Zip File 1 of 2 - Combined Data Files)_V2/Combined_m180c.dta")
  }
  
  hilda_l <- list("a2001" = df_a,
                  "b2002" = df_b,
                  "c2003" = df_c,
                  "d2004" = df_d,
                  "e2005" = df_e,
                  "f2006" = df_f,
                  "g2007" = df_g,
                  "h2008" = df_h,
                  "i2009" = df_i,
                  "j2010" = df_j,
                  "k2011" = df_k,
                  "l2012" = df_l,
                  "m2013" = df_m)
  
  if (FALSE) {
    name_file <- "/research/dataset/HILDA/HILDA_methods/hilda_names.xlsx"
    options(java.parameters = "-Xmx8000m")
    for(i in 1:length(hilda_l)) {
      label_i <- label(hilda_l[[i]])
      names_i <- colnames(hilda_l[[i]])
      name_df <- data.frame("WuEtAlVar" = NA, "Variable" = names_i, "Notes" = label_i)
      xlsx::write.xlsx(name_df, name_file, sheetName = names(hilda_l)[i], append = T, row.names = FALSE, showNA = F)
      gc()
    }
  }
}

#stage 2
if (TRUE) {
  master_file <- "/research/dataset/HILDA/HILDA_methods/hilda_vars_AL_v2.xlsx"
  master_vars <- read_excel(master_file, sheet = "Wu Name") %>%
    pull(1)
  hilda_l2 <- list()
  for (i in 1:13) {
    year_vars <- read_excel(master_file, sheet = (i+1),
                            col_types = c("text", 
                                          "text", "text")) %>%
      filter(WuEtAlVar %in% master_vars)
    t_df <- hilda_l[[i]] %>%
      select(all_of(year_vars$Variable))
    names(t_df) <- year_vars$WuEtAlVar
    t_df[t_df<0] <- NA
    t_df$xwaveid <- as.integer(t_df$xwaveid)
    t_df <- arrange(t_df, xwaveid)
    hilda_l2[[i]] <- t_df %>%
      as.data.table() %>%
      mutate(employmentstatus = as.factor(employmentstatus)) %>%
      one_hot() %>%
      as.data.frame()
  }
  
  personality_vars <- setdiff(names(hilda_l2[[13]]), names(hilda_l2[[12]]))
  #create time series
  other_vars <- setdiff(names(hilda_l2[[13]]), personality_vars) %>% setdiff("xwaveid")
  
  df1 <- rename_at(hilda_l2[[13]], vars(other_vars), ~ paste0(other_vars, "_m0"))
  for (i in 12:1) {
    m <- 13 - i
    t2_df <- hilda_l2[[i]]
    this_df_vars <- intersect(names(t2_df), other_vars)
    missing_df_vars <- setdiff(other_vars, names(t2_df))
    t2_df <- t2_df %>%
      select(c("xwaveid", this_df_vars)) %>%
      rename_at(vars(this_df_vars), ~ paste0(this_df_vars, "_m", m))
    
    if(length(missing_df_vars) > 0) {
      missing_df_vars <- paste0(missing_df_vars, "_m", m)
      t3_df <- matrix(data = NA, ncol = length(missing_df_vars), nrow = nrow(t2_df)) %>%
        data.frame()
      names(t3_df) <- missing_df_vars
      #create empty columns
      t2_df <- cbind(t2_df, t3_df)
    }
    df1 <- df1 %>%
      left_join(t2_df, by = c("xwaveid" = "xwaveid"))
  }
  
  df2 <- rename_at(hilda_l2[[9]], vars(other_vars), ~ paste0(other_vars, "_m0"))
  for (i in 8:1) {
    m <- 9 - i
    t2_df <- hilda_l2[[i]]
    this_df_vars <- intersect(names(t2_df), other_vars)
    missing_df_vars <- setdiff(other_vars, names(t2_df))
    t2_df <- t2_df %>%
      select(c("xwaveid", this_df_vars)) %>%
      rename_at(vars(this_df_vars), ~ paste0(this_df_vars, "_m", m))
    
    if(length(missing_df_vars) > 0) {
      missing_df_vars <- paste0(missing_df_vars, "_m", m)
      t3_df <- matrix(data = NA, ncol = length(missing_df_vars), nrow = nrow(t2_df)) %>%
        data.frame()
      names(t3_df) <- missing_df_vars
      t2_df <- cbind(t2_df, t3_df)
    }
    #create empty columns
    df2 <- df2 %>%
      left_join(t2_df, by = c("xwaveid" = "xwaveid"))
  }
  
  df3 <- rename_at(hilda_l2[[5]], vars(other_vars), ~ paste0(other_vars, "_m0"))
  for (i in 4:1) {
    m <- 5 - i
    t2_df <- hilda_l2[[i]]
    this_df_vars <- intersect(names(t2_df), other_vars)
    missing_df_vars <- setdiff(other_vars, names(t2_df))
    t2_df <- t2_df %>%
      select(c("xwaveid", this_df_vars)) %>%
      rename_at(vars(this_df_vars), ~ paste0(this_df_vars, "_m", m))
    
    if(length(missing_df_vars) > 0) {
      missing_df_vars <- paste0(missing_df_vars, "_m", m)
      t3_df <- matrix(data = NA, ncol = length(missing_df_vars), nrow = nrow(t2_df)) %>%
        data.frame()
      names(t3_df) <- missing_df_vars
      #create empty columns
      t2_df <- cbind(t2_df, t3_df)
    }
    df3 <- df3 %>%
      left_join(t2_df, by = c("xwaveid" = "xwaveid"))
  }
  
  { #users who completed all of Big Five survey in all three years
    t2005 <- hilda_l2[[5]] %>%
      mutate(extrav = rowMeans(.[grep("extrav*", names(.))], na.rm = T)) %>%
      mutate(agree = rowMeans(.[grep("agree*", names(.))], na.rm = T)) %>%
      mutate(consc = rowMeans(.[grep("consc*", names(.))], na.rm = T)) %>%
      mutate(neurotic = rowMeans(.[grep("neurotic*", names(.))], na.rm = T)) %>%
      mutate(open = rowMeans(.[grep("open*", names(.))], na.rm = T)) %>%
      select(c("xwaveid", "extrav", "agree", "consc", "neurotic", "open")) %>%
      na.omit()
    t2009 <- hilda_l2[[9]] %>%
      mutate(extrav = rowMeans(.[grep("extrav*", names(.))], na.rm = T)) %>%
      mutate(agree = rowMeans(.[grep("agree*", names(.))], na.rm = T)) %>%
      mutate(consc = rowMeans(.[grep("consc*", names(.))], na.rm = T)) %>%
      mutate(neurotic = rowMeans(.[grep("neurotic*", names(.))], na.rm = T)) %>%
      mutate(open = rowMeans(.[grep("open*", names(.))], na.rm = T)) %>%
      select(c("xwaveid", "extrav", "agree", "consc", "neurotic", "open")) %>%
      na.omit()
    t2013 <- hilda_l2[[13]] %>%
      mutate(extrav = rowMeans(.[grep("extrav*", names(.))], na.rm = T)) %>%
      mutate(agree = rowMeans(.[grep("agree*", names(.))], na.rm = T)) %>%
      mutate(consc = rowMeans(.[grep("consc*", names(.))], na.rm = T)) %>%
      mutate(neurotic = rowMeans(.[grep("neurotic*", names(.))], na.rm = T)) %>%
      mutate(open = rowMeans(.[grep("open*", names(.))], na.rm = T)) %>%
      select(c("xwaveid", "extrav", "agree", "consc", "neurotic", "open")) %>%
      na.omit()
    Reduce(intersect, list(t2005$xwaveid, t2009$xwaveid, t2013$xwaveid)) %>%
      length()
    
    c(t2005$xwaveid, t2009$xwaveid, t2013$xwaveid) %>%
      unique() %>%
      length()
  }
  

  df_combined <- df1 %>% 
    bind_rows(df2) %>%
    bind_rows(df3) %>%
    remove_empty("cols") %>%
    remove_empty("rows") %>%
    remove_constant(na.rm = TRUE)
  
  idx_all_traits_missing <- select(df_combined, personality_vars) %>%
    complete.cases()
  
  df_final <- df_combined[idx_all_traits_missing,] %>%
    mutate(extrav = rowMeans(.[grep("extrav*", names(.))], na.rm = T)) %>%
    mutate(agree = rowMeans(.[grep("agree*", names(.))], na.rm = T)) %>%
    mutate(consc = rowMeans(.[grep("consc*", names(.))], na.rm = T)) %>%
    mutate(neurotic = rowMeans(.[grep("neurotic*", names(.))], na.rm = T)) %>%
    mutate(open = rowMeans(.[grep("open*", names(.))], na.rm = T)) %>%
    select(-ends_with("_m0")) %>%
    my_shuffle()
}

if (FALSE) {
  my_rows <- lapply(hilda_l, nrow) %>%
    do.call(rbind, .)
  my_columns <- lapply(hilda_l, ncol) %>%
    do.call(rbind, .)

  my_years <- 2001:2013 %>%
    as.character()
  my_years[5] <- paste0(my_years[5], " $\\dagger$")
  my_years[9] <- paste0(my_years[9], " $\\dagger$")
  my_years[13] <- paste0(my_years[13], " $\\dagger$")
  
  my_df <- data.frame(Year = my_years, `Number of Observations` = my_rows, `Number of survey items` = my_columns)
  
  my_df %>%
    kbl(format="latex",
        booktabs = TRUE, linesep = "", row.names=FALSE, 
        caption = "HILDA longitudinal dataset statistics",
        col.names = c("Year", "Number of participants",   
                      "Number of survey items"), escape = F,
        label = "hilda",
        align = "lcc") %>%
    kable_styling(latex_options = c("striped"), font_size = 10, full_width = F) %>%
    footnote(c("$\\\\dagger$ Year personality survey was administered"), escape = F) %>%
    save_kable(hilda_file, 
               keep_tex = TRUE)
  
  my_df <- lapply(hilda_l, select, "xwaveid") %>%
    bind_rows(.id = "year")
  
  participation <- my_df %>%
    group_by(xwaveid) %>%
    summarize(N = n()) %>%
    select("N") %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(participation) <- c("nYears", "nParticipants")
  # make nYears an ordered factor
  participation$nYears <- factor(participation$nYears, levels = participation$nYears)
  
  p <- ggplot(participation, aes(x=nYears, y = nParticipants)) +
    geom_bar(stat="identity", fill="steelblue") +
    labs(x = "Number of HILDA waves completed", y = "Number of unique individuals") +
    theme_bw() +
    theme(text=element_text(size=40))
  ggsave(participation_image, width = 7, height= 5, units = "in", dpi = 300, plot = p)
  
  t <- round(participation$nParticipants[13]/sum(participation$nParticipants), 4)
  paste0("Participants completed all 13 surveys = ", t) %>%
    print()
}

#multicolinearity test
{
  my_df <- df_final %>%
    mutate(neurotic = rowMeans(.[grep("neurotic*", names(.))], na.rm = T))
  my_names <- names(my_df) %>%
    setdiff(personality_vars) %>%
    setdiff("neurotic")
  my_form <- paste0("neurotic~", paste0(my_names, collapse="+")) %>%
    as.formula()
  my_lm <- lm(my_form, my_df)
  mc_reg <- omcdiag(my_lm)
  my_df <- as.data.frame(print(mc_reg))
  
  my_df %>%
    kbl(format="latex",
        booktabs = TRUE, linesep = "", row.names=TRUE, 
        caption = "Overall Multicollinearity Diagnostics",
        label = "omc") %>%
    kable_styling(latex_options = c("striped"), font_size = 10, full_width = F) %>%
    footnote(c("1 --> COLLINEARITY is detected by the test ",
               "0 --> COLLINEARITY is not detected by the test")) %>%
    save_kable(omcdiag_file, 
               keep_tex = TRUE)
  
  bp_res <- bptest(my_lm)
  my_df <- as.data.frame(capture.output(print(bp_res)))
  
  paste0("Using the standard $lmtest$ package in $R$ studentized Breusch-Pagan test result [", 
         as.character(my_df[5,]),
         "] shows that the heteroskedasticity assumption has been violated") %>%
    writeLines(bptest_file)     
  
}

#Alpha
{
  keys.list <- list(extrav=grep("extrav*", names(df_final), value = T),
                    agree=grep("agree*", names(df_final), value = T),
                    consc=grep("consc*", names(df_final), value = T),
                    neurotic=grep("neurotic*", names(df_final), value = T), 
                    open = grep("open*", names(df_final), value = T)) 
  
  my_alpha <- function(my_keys, n, i) {
    vars <- my_keys[[i]]
    target_name <- n[i]
    my_df <- select(df_final, vars)
    cronbach_alpha5 <- alpha(my_df, keys = NULL, check.keys=TRUE, warnings=TRUE, impute = "medians")
    my_df_corrected <- reverse.code(cronbach_alpha5$keys, my_df)
    df_final[,target_name] <<- rowMeans(my_df_corrected, na.rm = TRUE)
    return(cronbach_alpha5$total)
  }
  
  alpha_df <- lapply(seq_along(keys.list), my_alpha, my_keys=keys.list, n=names(keys.list)) %>%
    do.call(rbind, .) 
  rownames(alpha_df) <- names(keys.list)
  
  alpha_df %>%
    round(3) %>%
    kbl(format="latex",
        booktabs = TRUE, linesep = "", row.names=TRUE, 
        caption = "Alpha table output from the R-psych package",
        label = "cronbachalpha") %>%
    kable_styling(latex_options = c("striped"), font_size = 10, full_width = T) %>%
    save_kable(cronbachalpha_file, 
               keep_tex = TRUE)
}

#EDA: Variability
if (FALSE) {
  p <- df_final %>%
    select(big5) %>%
    gather(Big5, value, c(big5)) %>%
    ggplot(aes(x = value, fill = Big5)) +
    geom_density(position = "identity", alpha = 0.4) +
    theme_bw() +
    theme(text = element_text(size = 50))
  ggsave(variability_image, width = 7, height= 5, units = "in", dpi = 300, plot = p)
  
  #Test for normality for neuroticism
  ad.test(df_final$neurotic)
  

  big5_subitem_wild <- paste0(big5, "*")
  big5_subitems <- grep(paste(big5_subitem_wild, collapse="|"), names(df_final), value = TRUE)
  INDEPENDANT_VARS <- names(df_final) %>%
    setdiff(big5_subitems)
  
  df_mh <- df_final %>%
    select(c(INDEPENDANT_VARS)) %>%
    remove_empty("rows") %>%
    remove_constant(na.rm = TRUE)
  df_mf.imputed <- df_mh %>%
    as.data.frame() %>%
    missRanger(verbose = 2,
               num.trees=500,
               maxiter=20,
               respect.unordered.factors=TRUE,
               splitrule = "extratrees")
  det.res <- MDmiss(df_mh,apply(df_mf.imputed,2,mean),var(df_mf.imputed))
  p <- PlotMD(dist = det.res, p = ncol(df_mh), alpha = 0.8)
  outind <- ifelse(det.res > p$halpha, TRUE, FALSE)
  # set NAs to FALSE
  outind[is.na(outind)] <- FALSE
  # how many outliers are there?
  sum(outind)
  
  df_mh.cleaned <- df_mh[outind==FALSE, ]
  df_mh.imp.cleaned <- df_mf.imputed[outind==FALSE, ]
  det.res.cleaned <- MDmiss(df_mh.cleaned, apply(df_mh.imp.cleaned,2,mean),var(df_mh.imp.cleaned))
  PlotMD(dist = det.res.cleaned, p = ncol(df_mh.cleaned), alpha = 0.9)
  
  outliers_list <- check_outliers(df_mf.imputed, method = "all") #"all"
  summary(outliers_list) %>% print()
  plot(outliers_list)
}


#EDA: 
#plot some charts
{
  pers_df <- select(df_final, big5) 
  pers_df <- pers_df[,order(colnames(pers_df))]
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(pers_df)
  M <- cor(pers_df)
  #png(height=8, width=8, file=corr_image, type = "cairo", res = 300, units = "in")
  #corrplot(M, type="upper", order="hclust", 
  #         p.mat = p.mat, sig.level = 0.01)
  #dev.off()
  ggcorrplot(M,
             #method = "circle",
             #colors = c("red", "green", "red"),
             type = "upper",
             hc.order = TRUE,
             p.mat = p.mat,
             lab = TRUE,
             lab_size = 7,
             sig.level = 0.01) +
    theme_bw() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 25))
  ggsave(corr_image, width = 2, height= 2, units = "in", dpi = 300)
}

{
  df_for_plots <- df_final
  df_for_plots$Gender <- as.factor(df_for_plots$sex_m0)
  df_for_plots$Age <- as.integer(df_for_plots$age_wave_m0)
  
  plot_normality(df_final, big5[1])
  plot_normality(df_final, big5[2])
  plot_normality(df_final, big5[3])
  plot_normality(df_final, big5[4])
  plot_normality(df_final, big5[5])
  
  gender <- target_by(df_for_plots, Gender)
  #
  {
    p1 <- plot(relate(gender, open)) + labs(title = "Openness by gender") + theme(plot.margin=unit(c(0.1,0,0,0.1),"cm"))
    p2 <- plot(relate(gender, extrav))  + labs(title = "Extraversion by gender") + theme(plot.margin=unit(c(0.1,0,0,0),"cm"))
    p3 <- plot(relate(gender, agree)) + labs(title = "Agreeableness by gender")+ theme(plot.margin=unit(c(0.1,0.1,0,0),"cm"))
    p4 <- plot(relate(gender, consc)) + labs(title = "Consciousness by gender")+ theme(plot.margin=unit(c(0,0,0,0.1),"cm"))
    p5 <- plot(relate(gender, neurotic)) + labs(title = "Neuroticism by gender")+ theme(plot.margin=unit(c(0,0,0,0),"cm"))
    p <- ggarrange(p1, p2, p3, p4, p5,
                   ncol=3, nrow=2, common.legend = TRUE, legend = "bottom") +
      theme_bw() +
      theme(text = element_text(size = 10))
    ggsave(big5_density_image, width = 7, height= 5, units = "in", dpi = 300, plot = p)
  }
  
  #Big5 by age
  {
    p1 <- ggplot(df_for_plots,mapping=aes(x=Age,y=open,color=Gender)) + geom_point(size=1, alpha = 0.1) + geom_smooth(method="lm") + 
      theme_bw() +
      theme(text = element_text(size = 20))
    p2 <- ggplot(df_for_plots,mapping=aes(x=Age,y=extrav,color=Gender)) + geom_point(size=1, alpha = 0.1) + geom_smooth(method="lm") + 
      theme_bw() +
      theme(text = element_text(size = 20)) 
    p3 <- ggplot(df_for_plots,mapping=aes(x=Age,y=agree,color=Gender)) + geom_point(size=1, alpha = 0.1) + geom_smooth(method="lm") + 
      theme_bw() +
      theme(text = element_text(size = 20)) 
    p4 <- ggplot(df_for_plots,mapping=aes(x=Age,y=consc,color=Gender)) + geom_point(size=1, alpha = 0.1) + geom_smooth(method="lm") + 
      theme_bw() +
      theme(text = element_text(size = 20)) 
    p5 <- ggplot(df_for_plots,mapping=aes(x=Age,y=neurotic,color=Gender)) + geom_point(size=1, alpha = 0.1) + geom_smooth(method="lm") + 
      theme_bw() +
      theme(text = element_text(size = 20)) 
    p <- ggarrange(p1, p2, p3, p4, p5,
                   ncol=3, nrow=2, common.legend = TRUE, legend = "bottom") +
      theme_bw()
    ggsave(big5_age_image, width = 7, height= 5, units = "in", dpi = 300, plot = p)
  }
}

#missingness chart
if (FALSE) {
  master_vars2 <- read_excel(master_file, sheet = "Wu Name") %>%
    select(c("WuVariable", "grouped"))
  big5_subitem_wild <- paste0(big5, "*")
  big5_subitems <- grep(paste(big5_subitem_wild, collapse="|"), names(df_final), value = TRUE)
  INDEPENDANT_VARS <- names(df_final) %>%
    setdiff(big5_subitems)
  
  missing.values <- df_final %>%
    select(INDEPENDANT_VARS) %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    mutate(key = str_remove(key, "_m.*")) %>%
    mutate(key = master_vars2$grouped[match(key, master_vars2$WuVariable)]) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  levels <-(missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  p <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(x ='Year-over-Year Variables',
         y = "% of missing values") +
    theme_bw() +
    theme(text = element_text(size = 40))
  
  ggsave(missingness_image, width = 7, height= 5, units = "in", dpi = 300, plot = p)
  
  t <- df_final %>%
    select(all_of(c(big5, INDEPENDANT_VARS))) %>%
    mcar_test()
  
  has_na <- sapply(df_final, function(x) any(is.na(x)))
  t2 <- TestMCARNormality(df_final[has_na], alpha = 0.01)
  print(t2)
}

{
  set.seed(101)  # good idea to set the random seed for reproducibility
  users <- unique(df_final$xwaveid)
  idx <- sample.split(users, SplitRatio = 0.9)
  users.train <- users[idx]
  users.test <- users[!idx]
  
  df.train <- df_final %>%
    filter(xwaveid %in% users.train) %>%
    select(-c("xwaveid"))
  df.test <- df_final %>%
    filter(xwaveid %in% users.test) %>%
    select(-c("xwaveid"))
  
  df.train.imputed <- df.train %>%
    as.data.frame() %>%
    missRanger(verbose = 2,
               num.trees=500,
               maxiter=20,
               respect.unordered.factors=TRUE,
               splitrule = "extratrees")
  
  combined_df <- rbind(df.train, df.test)
  
  combined_df_imputed <- combined_df %>% 
    as.data.frame() %>%
    missRanger(verbose = 2,
               num.trees=500,
               maxiter=20,
               respect.unordered.factors=TRUE,
               splitrule = "extratrees")
  df.test.imputed <- combined_df_imputed[(nrow(df.train)+1):nrow(combined_df),]
  
  save_list <- list(df.train = df.train,
                    df.test = df.test,
                    df.train.imputed = df.train.imputed,
                    df.test.imputed = df.test.imputed)
  
  saveRDS(save_list, SAVE_FILE)
}

save.image("/research/dataset/HILDA/HILDA_methods/cleaning.saved.RData")
