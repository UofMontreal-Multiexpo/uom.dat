## Other tests about functions from the file 'mixer.R'.
## Tests that cannot be automated using the testthat package.

library(uom.dat)


#### mcr_chart ####

## 1. mcr_chart returns an identical result whatever the structure of references
{
  values_list = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                     s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9))
  references_vector = c(a = 1, b = 2, c = 3)
  references_list = list(c(1,2), c(1,3), c(2,3), c(1,3))
  
  ## Expectation: the following instructions must return equivalent results
  
  mcr_chart(values_list, references_vector)
  mcr_chart(values_list, references_list)
}


## 2. mcr_chart returns an identical result whatever the chosen usage
{
  values_matrix = matrix(c(1,1, 0.5,0.5, 0.5,0.8, 0.9,0.9), ncol = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:2]))
  values_list = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                     s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9))
  references = c(a = 1, b = 1)
  
  ## Expectation: the following instructions must return equivalent results
  
  mcr_chart(values_matrix, references)
  mcr_chart(hi = hazard_index(values_matrix, references),
            mcr = maximum_cumulative_ratio(values_matrix, references),
            thq = top_hazard_quotient(values_matrix, references))
  
  mcr_chart(values_list, c(a = 1, b = 1, c = 1))
  mcr_chart(hi = sapply(values_list, hazard_index, c(1,1)),
            mcr = sapply(values_list, maximum_cumulative_ratio, c(1,1)),
            thq = sapply(values_list, top_hazard_quotient, c(1,1)))
}


## 3. mcr_chart plots 0 or 1 chart according to the argument plot
{
  values_list = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                     s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9))
  references_vector = c(a = 1, b = 2, c = 3)
  
  ## Expectation: plotting of 0 then 1, 1 and 1 plots
  
  chart = mcr_chart(values_list, references_vector, plot = FALSE)
  chart = mcr_chart(values_list, references_vector, plot = TRUE)
  mcr_chart(values_list, references_vector, plot = TRUE)
  mcr_chart(values_list, references_vector, plot = FALSE)
}


## 4. mcr_chart plots a logarithmic transformation of the graph according to the argument log_transform
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectations: 
  ## - Logarithmic transformations are applied to the X and Y axes if 'log_transform' is TRUE
  ## - An additional area is highlighted at the bottom of the graph if 'log_transform' is FALSE
  ## - Axis labels are adapted to the argument
  ## - Axis scales are adapted to the argument
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE)
}


## 5. mcr_chart displays colored areas according to the argument regions
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectations:
  ## - Areas are colored if 'regions' is TRUE
  ## - Areas are colored according to 'regions_col' if 'regions' is TRUE
  ## - Opacity of the areas increases along with 'regions_alpha'
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions = TRUE,
            regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"),
            regions_alpha = 0.2)
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions = TRUE,
            regions_col = c("#fdcc8a", "#fef0d9", "#fc8d59", "#d7301f"),
            regions_alpha = 0.2)
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions = TRUE,
            regions_col = c("#fdcc8a", "#fef0d9", "#fc8d59", "#d7301f"),
            regions_alpha = 0.6)
  
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions = TRUE,
            regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"),
            regions_alpha = 0.2)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions = TRUE,
            regions_col = c("#fdcc8a", "#fef0d9", "#fc8d59", "#d7301f"),
            regions_alpha = 0.2)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions = TRUE,
            regions_col = c("#fdcc8a", "#fef0d9", "#fc8d59", "#d7301f"),
            regions_alpha = 0.6)
}


## 6. mcr_chart displays labels identifying the regions according to the argument regions_lab
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectations:
  ## - Labels appear according to the order I, II, IIIA, IIIB
  ## - Labels are independant of the coloring parameter
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = TRUE,
            regions = FALSE)
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = c(TRUE, TRUE, FALSE, TRUE),
            regions = TRUE)
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = c(TRUE, FALSE, FALSE, TRUE),
            regions = FALSE)
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = c(TRUE, FALSE, FALSE, FALSE),
            regions = FALSE)
  
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = TRUE,
            regions = FALSE)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = c(TRUE, TRUE, FALSE, TRUE),
            regions = TRUE)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = c(TRUE, FALSE, FALSE, TRUE),
            regions = FALSE)
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = c(TRUE, FALSE, FALSE, FALSE),
            regions = FALSE)
}


## 7. mcr_chart plots points colored according to the argument thq_col
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectations:
  ## - Colors are assigned according to the top hazard quotients
  ## - Default colors exist
  ## - 'thq_col' do not need to be named
  ## - The given order in a named 'thq_col' does not matter
  ## - Color scale is ordered alphanumerically
  ## - Color scale only displays values related to the actual THQ
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            thq_col = NULL)
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            thq_col = c("red", "blue", "green"))
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            thq_col = c("red", "blue", "green"))
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            thq_col = c(b = "red", c = "blue", a = "green"))
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            thq_col = c(b = "red", c = "blue", a = "green"))
}


## 8. mcr_chart plots a linear regression according to the argument regression
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectation: if 'regression' is TRUE, a linear regression is represented with a confidence interval
  
  mcr_chart(values_matrix, references_vector,
            regression = FALSE)
  mcr_chart(values_matrix, references_vector,
            regression = TRUE)
}


## 9. mcr_chart ignore sets of values containing only values equal to 0
{
  values_matrix = matrix(c(0,0,0,
                           1,2,3), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectation: plotting a single point although there are two sets of values
  
  mcr_chart(values_matrix, references_vector)
}


## 10. mcr_chart displays MIAT group text according to the argument group_text
{
  values_matrix = matrix(c(1,   1.5, 0,
                           0.5, 0.2, 0,
                           0.5, 0.8, 0,
                           0.8, 0.8, 0.8), nrow = 4, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3", "s4"), letters[1:3]))
  references_vector = c(a = 1, b = 1, c = 1)
  
  ## Expectations:
  ## - Labels in chart refer to group or class
  ## - Legend title refers to group or class
  
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = TRUE, regions = TRUE,
            group_text = "group")
  mcr_chart(values_matrix, references_vector, log_transform = TRUE,
            regions_lab = TRUE, regions = TRUE,
            group_text = "class")
  
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = TRUE, regions = TRUE,
            group_text = "group")
  mcr_chart(values_matrix, references_vector, log_transform = FALSE,
            regions_lab = TRUE, regions = TRUE,
            group_text = "class")
}


