###################### packages ####################

#packages needed for this project
library(tidyverse)
library(Rtsne)
library(magrittr)
library(ggpubr)
library(kableExtra)
library(tidyquant)
library(ggdist)
library(ggthemes)


####################### colour scheme ###############

#set the colour scheme


col_sch <- c("#D81B87", "#FBBD89")



################# count instances function #######################

#count instances function

#function to count number of instances that are bigger than a certain number in a column
#x takes the input dataframe$column
#y takes any number as input
instances <- 0
count_inst <- function(col, y) {
  for (i in 1:length(col)) {
    if (col[i] > y) {
      instances = instances + 1
    }
  }
  return(instances)
}

######################## theme function ########################################

#theme

theme_cognitive <- function(){

  #use classic theme as base
  theme_classic() %+replace%

    theme(

      #specify components of title
      plot.title = element_text(
        size = 20, #font size
        face = 'bold', #print in bold
        hjust = 0.5, #central over plot
        vjust = 2),

      axis.title = element_text(
        size = 10),

      #specific components of tick labels on axes
      axis.text = element_text(
        margin = margin(5, b = 10),
        size = 10),
    )
}


########################### data import #####################################

# data import

file <- "data-raw/data.txt"
data <- read_table(file)


########################## data tidying ###############################
# data tidying

#select appropriate variables
data <- data %>%
  select(age,
         qd_dac,
         qd_adhd,
         qd_aq,
         gender,
         aq_social_skills = qd_aq_social,
         aq_attention_switching = qd_aq_switch,
         aq_attention_to_detail = qd_aq_detail,
         aq_communication = qd_aq_comm,
         aq_imagination_dimension = qd_aq_im,
         depression_score = qd_ces_d,
         maia_not_distracting = qd_maia_nd,
         maia_attention_regulation = qd_maia_ar,
         trait_anxiety = qd_tai,
         social_anxiety = qd_scsr_sa,
         vft_letters = vft_l_acc,
         vft_categories = vft_c_acc)


################################### add ID ####################################
#add a column called 'ID' as the first column to give each participant (row) an ID

#start at 1 and use the length of a column to get the right amount of rows
data <- data %>%
  add_column("id" = 1:nrow(data),
             .before = 1)

################################## remove participants #########################
#remove participants (rows) with too much missing data


#The id numbers have been identified in MATLAB. Those participants are missing data for more than 4 variables under investigation.
#The id numbers are stored in the variable "r_too_much_missing" in the MATLAB code that has been used for this project.

ids_delete <- c(36,59,62,64,65,70,72,73,93,95,98,99,101,105,120,140,141,142,155,165)

for(r in 1:length(ids_delete)) {
  for(c in 1:length(data$id))
    if(ids_delete[r] %in% data$id[c]) {
      data <- data[-c,]
    }
}



#####################replace missing values with column mean #############

#create variable to store the column mean
column_means <- NULL

#go through each column and calculate the mean for it and store it in the variable column_means
#as we're indexing column_means with the counter i, the mean for the first column will be stored in the first position of column_mean, the mean for the second column in the second position and so on.
for (n in 1:length(data)) {
  #we're storing the means in a variable so we have access to it in case we need it later
  column_means[n] <- sapply(data[,n],mean, na.rm = TRUE)

  #go through each value in the current column
  for (i in 1:length(data$id)) {

    #check whether the value is not a number, if it is not enter the conditional
    if (is.na(data[i,n])) {
      #replace the missing value with the mean
      data[i,n] <- column_means[n]
    }
  }
}

############################ get mean age and sex ###############################################


mean_age <- mean(data$age)
mean_age

#count how many identified as male (0) and female (1)
count(data, cols = gender)



############################# participants exceeding cut-off #############################

#count how many participants exceed dyslexia cut off
dys_pp <- count_inst(data$qd_dac, 45)

#count how many participants exceed autism specturm quotient cut-off
asq_pp <- count_inst(data$qd_aq, 31)


################################### categorical dyslexia ######################################

#create categorical variable from dyslexia score (qd_dac)

#create new column to store the categorical information of the dyslexia score in
data <- data %>%
  add_column("dyslexia" = 1:length(data$qd_dac),
             .after = 3)

#go through each value in the dyslexia score column
for (i in 1:length(data$qd_dac)) {

  #if the current value exceeds the cut-off of 45, add 'dyslexia' to that participant in the new column
  if (data$qd_dac[i] > 45) {
    data$dyslexia[i] = "dyslexia"
  }

  #if the cut-off is not exceeded, add 'no dyslexia' to that participant in the column
  else {
    data$dyslexia[i] = "no dyslexia"
  }
}



############################ general subset (match MATLAB) ################################



#select same data as used for SOM generation in MATLAB plus categorical dyslexia variable to colour graphs
matlab_data <- data %>%
  select(dyslexia,
         aq_social_skills,
         aq_attention_switching,
         aq_attention_to_detail,
         aq_communication,
         aq_imagination_dimension,
         depression_score,
         maia_not_distracting,
         maia_attention_regulation,
         trait_anxiety,
         social_anxiety,
         vft_letters,
         vft_categories)

