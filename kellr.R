### kellr



# test data frame ####
# tdf <- data.frame(a = 1:6, 
#                   b = 2:7, 
#                   c = 3:8, 
#                   d = c("f", "m", "f", "m", "f", "f"), 
#                   e = c(T, T, T, F, F, F), 
#                   f = c("A", "B", "B", NA, "", "C"),
#                   g = seq(Sys.Date(), Sys.Date() + 5, 1),
#                   h = rnorm(6),
#                   i = factor(c("do", "re", "mi", "fa", "sol", "la")))

# double rows of test data frame i times ####
# for (i in 1:12) tdf <- rbind(tdf, tdf)





# round_format:  rounds and formats to 2 decimals ####
# round_format <- function(x, decimals = 2) gsub(" ", "", format(round(x, decimals), nsmall = decimals))




# nas:  count NAs ####
# nas <- function(x) {list(n = sum(is.na(x)),
#                          pct = paste0(round(sum(is.na(x))/length(x) * 100, 3), "%"))}




# lunique:  count unique values -> lenght(unique()) ####
lunique <- function(x) {
  length(unique(x))
}




# names_with/out:  find column names with "x" and names without "x" ####
names_with <- function(x, df) names(df)[grepl(x, names(df), fixed = T)]
names_without <- function(x, df) names(df)[!grepl(x, names(df), fixed = T)]





# unique_names: names that are unique to first dataframe as compared to second dataframe ####
# unique_names <- function(x, y) {
#   names(x)[!names(x) %in% names(y)]
# }

# common_names: names that common to both dataframes ####
# common_names <- function(x, y) {
#   names(x)[names(x) %in% names(y)]
# }

# names_are: find columns that are certain class #####
# names_are <- function(x, df) names(df)[unlist(lapply(df, function(var) class(var) == x), use.names = T)]





# search whole data frame for string ####
# search_data <- function(x, df) {
#   temp <- df %>%
#     select_if(~ sum(grepl("afri", .x), na.rm = T) > 0)
#   names(temp)
# }


# clist:  read c("x, y, z") as c("x", "y", "Z") ####
clist <- function(x, sep = ", ") {
  strsplit(
    gsub(",", ", ", 
         gsub("\n", "", 
              gsub(" ", "", x, fixed = T), fixed = T), fixed = T), 
  sep)[[1]]
}




# varchar:  variable characteristics ####
# varchar <- function(x) {
#   list(
#     class = class(x),
#     unique = paste0(length(unique(x)), " unique, ", length(x), " length"),
#     min_max = paste0(min(x, na.rm = T), ", ", max(x, na.rm = T)),
#     nas_or_empty = paste0(sum(is.na(x)) + sum(x == "", na.rm = T), ", ", round((sum(is.na(x)) + sum(x == "", na.rm = T)) / length(x) * 100, 2), "%"),
#     head = head(x)
#   )
# }





# table_na:  table with useNA = "always" as default ####
# table_na <- function(x) {
#   r <- table(x, useNA = "always")
#   names(attributes(r)$dimnames) <- NULL
#   r
# }






# mean_sd:  "mean (sd)" of x ####
# mean_sd <- function(x, decimals = 2) {
#   paste0(format(round(mean(x, na.rm = T), decimals), nsmall = decimals), " (", format(round(sd(x, na.rm = T), decimals), nsmall = decimals), ")")
# }





# determines order for variables structured "number (percentage%)" ####
# for (name in names(dados)[2:length(names(dados))]) {
#   dados[[name]] <- factor(dados[[name]], 
#                           levels = unique(dados[[name]][order(as.numeric(sapply(dados[[name]], function(x) gsub("\\%\\)", "", strsplit(x, "\\(")[[1]][2]))),
#                                                               as.numeric(sapply(dados[[name]], function(x) strsplit(x, " ")[[1]][1])))]))
# }




# pct/table_pct:  percentages of x unique values ####
# pct <- function(x, decimals = 2) {
#   r <- round(prop.table(table(x)) * 100, decimals)
#   names(attributes(r)$dimnames) <- NULL
#   r
# }

# table_pct <- function(x, decimals = 2) {
#   p <- round(prop.table(table(x)) * 100, decimals)
#   names(attributes(p)$dimnames) <- NULL
#   t <- table(x)
#   names(attributes(t)$dimnames) <- NULL
#   list(pct = p,
#        table = t)
# }




# table_descriptives:  table with descriptives for continuous [mean (sd)] and dichotomous [%] variables in a data set ####
# table_descriptives <- function(data, decimals = 2) {
#   
#   data_tab <- data.frame(matrix(nrow = 1, ncol = ncol(data)))
#   names(data_tab) <- names(data)
#   for (i in names(data)) {
#     
#     if (class(data[[i]]) %in% c("numeric", "integer")) {
#       
#       data_tab[[i]] <- paste0(format(round(mean(data[[i]], na.rm = T), decimals), nsmall = decimals), " (", format(round(sd(data[[i]], na.rm = T), decimals), nsmall = decimals), ")")
#       
#     } else if (class(data[[i]]) %in% c("factor", "character", "logical")) {
#       
#       data_tab[[i]] <- paste0(format(round(as.numeric(prop.table(table(data[[i]]))[1]) * 100, decimals), nsmall = decimals), "% ", names(prop.table(table(data[[i]]))[1]))
#       
#     } else {
#       
#       data_tab[[i]] <- "invalid class"
#       
#     }
#     
#   }
#   
#   return(data_tab)
#   
# }



# table_descriptives_grouped:  table_descriptives grouped by variable ####
# table_descriptives_grouped <- function(data, grouping_var, decimals = 2) {
#   
#   
#   table_descriptives <- function(data, decimals = 2) {
#     
#     data_tab <- data.frame(matrix(nrow = 1, ncol = ncol(data)))
#     names(data_tab) <- names(data)
#     for (i in names(data)) {
#       
#       if (class(data[[i]]) %in% c("numeric", "integer")) {
#         
#         data_tab[[i]] <- paste0(format(round(mean(data[[i]], na.rm = T), decimals), nsmall = decimals), " (", format(round(sd(data[[i]], na.rm = T), decimals), nsmall = decimals), ")")
#         
#       } else if (class(data[[i]]) %in% c("factor", "character", "logical")) {
#         
#         data_tab[[i]] <- paste0(format(round(as.numeric(prop.table(table(data[[i]]))[1]) * 100, decimals), nsmall = decimals), "% ", names(prop.table(table(data[[i]]))[1]))
#         
#       } else {
#         
#         data_tab[[i]] <- "invalid class"
#         
#       }
#       
#     }
#     
#     return(data_tab)
#     
#   }
#   
#   
#   data_tab_grouped <- NULL
#   for (i in unique(data[[grouping_var]])) {
#     
#     temp <- data[data[[grouping_var]] == i, ]
#     
#     data_tab_grouped <- rbind(data_tab_grouped, table_descriptives(temp))
#     
#   }
#   
#   data_tab_grouped[[grouping_var]] <- sapply(strsplit(data_tab_grouped[[grouping_var]], "% "), function(x) x[2])
#   
#   data_tab_grouped <- cbind(data_tab_grouped[[grouping_var]], data_tab_grouped[, names(data_tab_grouped)[names(data_tab_grouped) != grouping_var ]])
#   
#   names(data_tab_grouped)[1] <- grouping_var
#   
#   return(data_tab_grouped)
#   
#   
# }






# fix_special_chars:  fix special characters ####
# fix_special_chars <- function(data) {
#   
#   for (variable in names(data)) {
#     
#     data[[variable]] <- gsub("\xfc", "ü", data[[variable]])
#     data[[variable]] <- gsub("<fc>", "ü", data[[variable]])
#     data[[variable]] <- gsub("<e4>", "ä", data[[variable]])
#     data[[variable]] <- gsub("\xe4", "ä", data[[variable]])
#     data[[variable]] <- gsub("\xc4", "Ä", data[[variable]])
#     data[[variable]] <- gsub("\xf6", "ö", data[[variable]])
#     data[[variable]] <- gsub("<f6>", "ö", data[[variable]])
#     data[[variable]] <- gsub("<d6>", "Ö", data[[variable]])
#     data[[variable]] <- gsub("<df>", "ß", data[[variable]])
#     data[[variable]] <- gsub("<e1>", "á", data[[variable]])
#     data[[variable]] <- gsub("<e7>", "ç", data[[variable]])
#     data[[variable]] <- gsub("<e3>", "ã", data[[variable]])
#     data[[variable]] <- gsub("<ed>", "í", data[[variable]])
#     data[[variable]] <- gsub("<a0>", "\t", data[[variable]])
#     data[[variable]] <- gsub("<e9>", "é", data[[variable]])
#     data[[variable]] <- gsub("<e0>", "à", data[[variable]])
#     data[[variable]] <- gsub("<ea>", "ê", data[[variable]])
#     data[[variable]] <- gsub("<93>", "“", data[[variable]])
#     data[[variable]] <- gsub("<94>", "”", data[[variable]])
#     data[[variable]] <- gsub("<fa>", "ú", data[[variable]])
#     data[[variable]] <- gsub("<f3>", "ó", data[[variable]])
#     data[[variable]] <- gsub("<c0>", "À", data[[variable]])
#     data[[variable]] <- gsub("<f5>", "õ", data[[variable]])
#     data[[variable]] <- gsub("<e2>", "â", data[[variable]])
#     
#   }
#   
#   return(data)
#   
# }







# remove_attr:  remove all attributes from variables ##### DOESNT WORK #####
# remove_attr <- function(df) {
#   for (var in names(df)) {
#     attrs <- names(attributes(df[[var]]))
#     for (attribute in attrs) {
#       attr(df[[var]], attribute) <- NULL
#     }
#   }
# }




# cdbk:  generate a codebook for a data frame ####

# cdbk <- function(data) {
#   
#   cdbk.vars <- NULL
#   var <- NULL
#   for (i in names(data)) {
#     var$varname <- i
#     var$class <- class(data[[i]])[1]
#     var$n_unique <- length(unique(data[[i]]))
#     var$nas <- paste0(sum(is.na(data[[i]])), ", ", round((sum(is.na(data[[i]])) / length(data[[i]])) * 100, 1), "%")
#     var$empty <- paste0(sum(data[[i]] == "", na.rm = T), ", ", round((sum(data[[i]] == "", na.rm = T) / length(data[[i]])) * 100, 1), "%")
#     var$min_max <- paste0(min(data[[i]], na.rm = T), ", ", max(data[[i]], na.rm = T))
#     var$mean <- ifelse(is.numeric(data[[i]]), mean(data[[i]], na.rm = T), NA)
#     var$median <- ifelse(is.numeric(data[[i]]), median(data[[i]], na.rm = T), NA)
#     var$sd <- ifelse(is.numeric(data[[i]]), sd(data[[i]], na.rm = T), NA)
#     var$prop.dichotomous <- ifelse(length(unique(data[[i]][!is.na(data[[i]])])) == 2, 
#                                    paste0(unique(data[[i]][!is.na(data[[i]])])[1],
#                                           " -> ",
#                                           round((sum(data[[i]] == unique(data[[i]][!is.na(data[[i]])])[1], na.rm = T) / length(data[[i]])) * 100, 1),
#                                           "%"),
#                                    NA)
#     var$unique_cases <- paste0(head(unique(data[[i]]), 
#                                     ifelse(lunique(data[[i]]) > 10, 10, lunique(data[[i]]))), 
#                                collapse = ", ")
#     var$head <- paste0(head(data[[i]]), collapse = ", ")
#     
#     
#     
#     for (name in names(var)) {
#       cdbk.vars[[name]] <- c(cdbk.vars[[name]], var[[name]])
#     }
#   }
#   
#   cdbk.vars <<- as.data.frame(cdbk.vars)
#   
# }



# generate a simulated replica of a data frame using descriptive stats ####







# for loop that stores t-test results in data.frame ####

# ttests <- NULL
# ttest <- NULL
# for (var in VARIABLES) {
#   
#   t <- t.test(df[[var]] ~ df[["OUTCOME VARIABLE"]])
#   ttest$var <- var
#   ttest$LowDR <- round(t$estimate[1], 2)
#   ttest$HighDR <- round(t$estimate[2], 2)
#   ttest$d <- round(ttest$HighDR - ttest$LowDR, 2)
#   ttest$p <- round(t$p.value, 4)
#   
#   for (name in names(ttest)) {
#     ttests[[name]] <- c(ttests[[name]], ttest[[name]])
#   }
#   
# }
# 
# ttests <- as.data.frame(ttests)


# recode Stata variable as character variable with labels ####

# x <- as.character(factor(x, levels = attr(x, "labels"), labels = names(attr(x, "labels"))))


# read data in any format ####

# if (grepl(".csv", i)) {
#   temp <- read.csv(i, stringsAsFactors = F)
# } else if (grepl(".xlsx", i)) {
#   temp <- read_xlsx(i)
# } else if (grepl(".dta", i)) {
#   temp <- import(i)
# } else if (grepl(".sav", i)) {
#   temp <- read_sav(i)
# }




# prep.rbind: makes two data frames have the same columns by adding empty columns to each #####
prep.rbind <- function(a, b) {
  
  if (length(b) > 0) {
    for (i in names(a)) {
      if(!i %in% names(b)) {
        b[[i]] <- NA
      }
    }
  }
  if (length(a) > 0) {
    for (i in names(b)) {
      if(!i %in% names(a)) {
        a[[i]] <- NA
      }
    }
  }
  
  rbind(a, b)
  
}


  
  
  