# List necessary packages
packages_list<-list("magrittr", "dplyr", "plyr", "MatchIt", "RItools", "Hmisc", "this.path", "scales", "ggdendro", "data.table", "openxlsx",
                    "tibble", "leaps", "pbapply", "RColorBrewer", "ggpubr", "ggdist")

# Install necessary packages not installed
packagesPrev<- .packages(all.available = TRUE)
lapply(packages_list, function(x) {   if ( ! x %in% packagesPrev ) { install.packages(x, force=T)}    })

# Load libraries
lapply(packages_list, library, character.only = TRUE)

# Set working directory
# Uses 'this.path::this.path()' to find the path of the current R script and 'dirname()' to get its directory. This directory is set as the working directory where the current code is stored.
dir_work<- this.path::this.path() %>% dirname()

# Set input - output folder
# Establishes paths for input and output directories relative to 'dir_work'. 'file.path()' constructs the path to the directories ensuring it is OS independent.
input<- file.path(dirname(dir_work), "input")
output<- file.path(dirname(dir_work), "output")


# Load data ####

# Load data. The data should be a table where rows correspond to spatial units (e.g., pixels) and columns represent variables.
# Each spatial unit (row) will serve as the input for the matching process, which will be performed based on the similarity between covars (columns)
setwd(input)
data<- read.csv("data.csv", header = T)

names(data)
head(data)

# Specify the column name in data that defines spatial units in relation to governance type. 
# This column indicates which spatial units are associated with a type of governance (1) and which are not (0).
type_gov<- "Type"
table(data[,type_gov])

# List preliminary covariates-columns in data to estimate similarity for matching.
# These are considered preliminary as they will undergo multicollinearity tests and significance checks in relation to governance type.
covars<-c( "Department", "Anual_Prec", "Prec_Seas",  "Dis_Def",    "Dis_Rivers",
          "District_R", "Departme_R", "National_R", "D7Set10",    "D7Set1000",  "D7Set5000",  "D7Set10000", "D17Set10",   "D17Set1000", "D17Set5000",
          "D17Set10000", "Ecoregions", "Elevation",  "Pop2000",    "Pop2020",    "Slope",      "Tra_Time00", "Tra_Time15")


# test multicolinearity ####
  
# Evaluate multicollinearity: It can distort interpretations by inflating the variance of regression coefficients of variables
# For matching analysis, we use the glm typegov ~ covar1 + covar2 + covar3 with a binomial distribution to model the probability of governance based on covariates. This formula can also be used to assess multicollinearity, demonstrating how covariate interactions contribute to variance inflation in relation to typegov by mean of the same glm test.

formula_test_multicor<- as.formula( paste0(type_gov, "~", paste0(covars, collapse = "+")) )
test_multicor<- glm(formula_test_multicor, data = data, family = binomial()) # sort by inflacion de varianza

print(formula_test_multicor)
test_multicor

# The results of this  model are organized as a correlation matrix, which displays multicollinearity among covariates with respect to the response variable typegov.
cordataR<- summary(test_multicor, correlation=T)[["correlation"]] %>% as.data.frame.matrix()
cordataR[,"(Intercept)"]<- NULL; cordataR<- cordataR[2:nrow(cordataR), ]# ELIMINAR INTERCEPT MATRIZ DE CORRELACION
NACol<- names(which(rowSums(is.na(cordataR)) > (ncol(cordataR)/2) ))
cordata<- cordataR %>% {.[!names(.) %in% NACol,]} %>% {.[,!colnames(.) %in% NACol]}; cordata[is.na(cordata)]<-0

str(cordata)
head(cordata)


# From the correlation matrix, we must decide which variables to remove to reduce multicollinearity. To achieve this, we generate groups of correlated variables using a correlation threshold.
cor_threshold<- 0.65 # define correlation threshold

# Covariate clustering
corhclust <- hclust(as.dist(1-abs(cordata))) 
cordend<-as.dendrogram(corhclust)
cordend_data <- dendro_data(cordend)

# Plot dendrogram of correlated variables. The dendrogram visualizes the correlation among variables, highlighting groups of correlated covariables based on the defined correlation threshold (cor_threshold) marked by a red line.
var_table <- with(cordend_data$labels, data.frame(y_center = x, y_min= x-0.5, y_max=x+0.5, Variable = as.character(label), height = 1))
col1<- "#EBEBEB"; col2<- "white"; is.even<- function(x){ x %% 2 == 0 }
var_table$col<- rep_len(c(col1, col2), length.out=length(var_table$Variable)) %>% {if(is.even(length(.))) {rev(.)} else {.}}
segment_data <- with(segment(cordend_data), data.frame(x = y, y = x, xend = yend, yend = xend, cor= 1-yend))

ggdendroPlot <-   ggplot()+
  annotate("rect", xmin = -0.05, xmax = 1.04, fill = var_table$col,ymin = var_table$y_min , ymax = var_table$y_max, alpha = 0.75 )+
  geom_segment(data= segment_data, aes(x = 1-x, y = y, xend = 1-xend, yend = yend, label= cor), size= 0.3)+
  scale_y_continuous(breaks = cordend_data$labels$x,  labels = cordend_data$labels$label )+
  coord_cartesian(expand = F)+
  labs(x= "Correlation", y= "Variables") +
  geom_vline(xintercept = cor_threshold, linetype = "dashed", col= "red") +
  theme(legend.position =  "bottom", legend.key.width = unit(50, 'pt'),
        plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
        panel.grid.major = element_line(color = "gray"),
        axis.ticks.length   = unit(0.3, "mm"),
        text = element_text(size = 10))

ggdendroPlot

# Remove high-correlated variables. Following exploration, we select one variable per group to reduce multicollinearity, choosing the variable with the lowest VIF in each group. The VIF indicates the level of multicollinearity; thus, keeping the variable with the lowest VIF in each group to effectively reduce multicollinearity.
vif_data<- car::vif(test_multicor) %>% as.data.frame() %>% {data.frame(Var= rownames(.), VIF= .[,1])} %>% arrange(VIF)
vif_values <-  vif_data %>% 
    dplyr::mutate(Variable1= Var, VIF_var1= VIF, Variable2= Var, VIF_var2= VIF) %>%  dplyr::arrange("VIF")
rank_covars<- cutree(corhclust, h = 1-cor_threshold) %>% as.data.frame %>% rownames_to_column("Var") %>% setnames(".", "group") %>%
      dplyr::filter(!Var %in% "(Intercept)") %>% list(vif_data) %>% join_all() %>% arrange(group, VIF)
    
covars_no_multicol<- dplyr::filter(rank_covars, !duplicated(group))$Var

rank_covars
covars_no_multicol
  
  
# Optimization and adjustment for selecting the best model ####
pre_formula_glm<- as.formula( paste0(type_gov, "~", paste0(covars_no_multicol, collapse = "+")) ) # new formula with variables that do not exhibit multicollinearity
  
# Estimate forward and backward models. This technique iteratively adds and removes variables to find the optimal model that balances complexity with predictive power. We calculate "rsq" (R-squared), "rss" (Residual Sum of Squares), "adjr2" (Adjusted R-squared), "cp" (Mallows' Cp), "bic" (Bayesian Information Criterion), and AIC (Akaike Information Criterion). Each criterion offers a different perspective on model performance, balancing complexity against the risk of overfitting. By default we use AIC to select the better models.
model  <- regsubsets(pre_formula_glm, data = data, nvmax = length(covars), method = "seqrep")
summ_model<-summary(model)[c("rsq", "rss", "adjr2", "cp", "bic" )] %>% as.data.frame() %>% dplyr::mutate(model= seq(nrow(.)))
list_models<- seq(nrow(summ_model))
AIC_models<- pblapply(list_models, function(x){
    coefs<- coef(model, id = x) # get coefficients
    vars<- names(coefs)[-1] # get vars
    form_test<- as.formula( paste0(type_gov, "~", paste0(vars, collapse = "+")) ) # organize form
    glm_test<- glm(form_test, data = data, family = binomial()) # run glm
    data_AIC<- data.frame(model= x, AIC= extractAIC(glm_test)[2]) # get AIC
    data_vars <- data.frame(model= x, vars= vars) # get data vars
    data_form<- data.frame(model= x, formula = paste0(type_gov, "~", paste0(vars, collapse = "+")) ) # get data forms
    list(data_AIC=data_AIC, data_vars=data_vars, data_form= data_form )   })
  
  
# Ranking mejores modelos
# Compiles the formulas of models evaluated by AIC into a single dataframe for easy comparison.
forms_models<- rbind.fill(purrr::map(AIC_models, "data_form"))
forms_models

# Aggregates AIC information, merges it with summary model data, and ranks models based on BIC and AIC values to identify the best performers.
better_models<- rbind.fill(purrr::map(AIC_models, "data_AIC")) %>% list(summ_model) %>% join_all() %>% 
    dplyr::arrange( bic) %>% dplyr::mutate(rank_BIC= seq(nrow(.))) %>% 
    dplyr::arrange(AIC) %>% dplyr::mutate(rank_AIC= seq(nrow(.)))
  
print(better_models)

# Selection of the best model
# Allows the researcher to review the ranked models based on selected criteria (default: AIC)
critera<- "AIC" # DEFAULT


# Collates variables from the best AIC models, ranks them by frequency of appearance, and prepares them for visualization analysis.
data_vars<-  rbind.fill(purrr::map(AIC_models, "data_vars")) %>% list(better_models) %>% 
    join_all() %>%  dplyr::group_by(vars) %>% dplyr::mutate(freq_var= n()) %>% 
    dplyr::arrange(freq_var) %>% dplyr::mutate(vars= factor(vars, levels = unique(.$vars)) ) 
  
# Organizes and ranks models based on the selected criterion preparing for the selection of the best model.
vars_models<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
    dplyr::mutate(model= factor(model, levels = unique(.$model)) ) %>% as.data.frame()
  

# Identifies the best model based on the ranking, setting it aside for in-depth analysis and use in matching.
better_model<- unique(vars_models[1,1])
better_model
  
# Selected variables for the best model
selected_variables<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
  dplyr::filter(model %in% better_model) %>% {as.character(.$vars)}
  
  
# Plot best models' AIC by variable. The heatmap displays the top-performing models horizontally and the most impactful variables vertically. Warmer hues indicate superior AIC values, illustrating the effectiveness of each variable within the highest-ranked models as per the chosen metric (e.g., AIC). 
plot_better_model<-  ggplot()+
    geom_tile(data= vars_models, aes(x= model , y= vars, fill = eval(sym(critera)) ), color="black", alpha= 0.5, size=0.2)+
    scale_fill_gradientn(critera, colors = brewer.pal(11, "Spectral"))  
  
plot_better_model
  
  
# Pre-matching exploratory analysis. ####
# This phase involves analyzing distributions and assessing the balance of variables across the typegov groups. It provides an initial diagnostic of the data, setting the stage for understanding the impact of the matching process. 


# Propensity scores calculation. Propensity scores estimate the probability of treatment assignment based on observed covariates
# Identifies the 'treated' group based on 'type_gov' == 1, then calculates the standardized differences for selected variables. These differences help in assessing the balance between treated and untreated groups before matching.

treated <-(data[,type_gov] ==1) 
cov <-data[,selected_variables]
std.diff <-apply(cov,2,function(x) 100*(mean(x[treated])- mean(x[!treated]))/(sqrt(0.5*(var(x[treated]) + var(x[!treated]))))) %>% abs()
  
# Generate a propensity score model
formula_glm<- as.formula( paste0(type_gov, "~", paste0(selected_variables, collapse = "+")) )
ps <- glm(formula_glm, data = data, family = binomial())

# Estimate propensity scores predicted by the logistic model.
data$psvalue <- predict(ps, type = "response")

# Organizes standardized differences for easy analysis and visualization, highlighting variables with the most imbalance.
PreMatchingIndexData<- data.frame(abs(std.diff)) %>% set_names("imbalance") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(imbalance)

# A good balance is considered to be less than 25%. For visualization purposes, imbalances greater than 100% are capped at 100%. # An imbalance over 25% indicates significant differences in group characteristics, necessitating adjustment for unbiased causal inference.
PreMatchingIndexDataV2<- PreMatchingIndexData  %>%  arrange(desc(imbalance)) %>%
    mutate(imbalance= ifelse(imbalance>=100,100,imbalance)) %>% 
    mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))
  
PreMatchingIndexDataV2
  
# Execute matching analysis ####
# Performs matching based on the specified criteria to balance the treatment and control groups, enhancing the validity of causal inferences. 
# The 'formula_glm' defines the treatment indicator and covariates for matching, 'method = "nearest"' specifies nearest neighbor matching to pair individuals based on similarity in propensity scores, and 'ratio = 1' ensures a 1:1 match between treated and untreated units, aiming for the closest match possible without replacement.
m.nn <- matchit(formula_glm, data =data, method= "nearest", ratio = 1)

# Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise. This step prepares the data for subsequent analysis of treatment effects on deforestation.
y=match.data(m.nn, group="all")

# Propensity scores calculation. 
treated1 <-(y[, type_gov]==1)
cov1 <-y[, selected_variables]
std.diff1 <-apply(cov1,2,function(x) 100*(mean(x[treated1])- mean(x[!treated1]))/(sqrt(0.5*(var(x[treated1]) + var(x[!treated1]))))) 
  

  
# Organizes standardized differences for easy analysis and visualization
posmatchingIndexData<- data.frame(abs(std.diff1)) %>% set_names("imbalance") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(imbalance)
posmatchingIndexDataV2<- posmatchingIndexData  %>%  arrange(desc(imbalance)) %>%
  mutate(imbalance= ifelse(imbalance>=100,100,imbalance)) %>% 
  mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))

# Estimate percent balance improvement. Organize the standardized differences before and after matching data.
summ_Imbalancedata<- plyr::rbind.fill(list( dplyr::mutate(PreMatchingIndexDataV2, Match= "Unmatched"),  dplyr::mutate(posmatchingIndexDataV2, Match= "Matched")))

  
# Plotting imbalance data. The plot demonstrates the improvements in variable imbalance before and after matching.

y_axis_title_unbalanced_vars_posmatch<- "Index of covariate imbalance"
x_axis_title_unbalanced_vars_posmatch<- "Variables"
legend_title_unbalanced_vars_posmatch<- "Pixeles de la ventana"
plot_title_unbalanced_vars_posmatch<- "Pos Matching"
color_vline_unbalanced_vars_posmatch<- "red"
pos_vline_unbalanced_vars_posmatch<- 25

gg_summ_Imbalancedata<- ggplot(data= summ_Imbalancedata)+  
  geom_point(aes(x=imbalance, y= Variable, color= Match), size= 1) +
  labs(x= y_axis_title_unbalanced_vars_posmatch, y= x_axis_title_unbalanced_vars_posmatch)+
  geom_vline(aes(xintercept= pos_vline_unbalanced_vars_posmatch),  size= 0.5, linetype="dashed", color = color_vline_unbalanced_vars_posmatch)+
  theme(
    plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
    axis.ticks.length   = unit(0.3, "mm"),
    text = element_text(size = 10),
    panel.background = element_rect(fill = NA), panel.grid.major = element_line(color = "gray"),
    axis.line = element_line(size = 0.5, colour = "black") )+
  scale_x_continuous(expand = c(0,0), limits = c(0,110))+
  ggtitle(type_gov)
  
gg_summ_Imbalancedata



# Plotting Imbalance Figure. This plot visualizes the distribution of propensity scores before and after matching through histograms. It helps to assess the effectiveness of the matching process in balancing the covariates between treated and untreated groups.

# Splitting the pre-matching data based on the 'type_gov' variable to compare groups inside and outside the treatment condition.
includedListPreMatching <- split(data, data[, type_gov])

# Set graphic parameters for pre-matching histograms.
in_area_prematch <- list(title= paste0("In ", type_gov), color= "goldenrod")
out_area_prematch <- list(title= paste0("Out ", type_gov), color= "olivedrab")
y_axis_title_prematch <- "Number of Units"
x_axis_title_prematch <- "Propensity Score"
legend_title_prematch <- "Window Pixels"
plot_title_prematch <- "Pre Matching"

# Create pre-matching plot.
PreMatchversusplot <- ggplot(data = includedListPreMatching[["0"]]) +
  geom_histogram(data = includedListPreMatching[["1"]], aes(x = psvalue, y = ..count.., 
                                                            fill = in_area_prematch$title, color = in_area_prematch$title)) +
  geom_histogram(aes(x = psvalue, y = -..count.., fill = out_area_prematch$title, color = out_area_prematch$title)) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = alpha(c(out_area_prematch$color, in_area_prematch$color), 0.5), name = legend_title_prematch) +
  scale_color_manual(values = alpha(c(out_area_prematch$color, in_area_prematch$color), 1), name = legend_title_prematch) +
  labs(x = x_axis_title_prematch, y = y_axis_title_prematch, title = plot_title_prematch) +
  guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 10))

# Splitting the post-matching data.
includedListposmatching <- split(y, y[, type_gov])

# Set graphic parameters for post-matching histograms.
in_area_posmatch <- list(title= paste0("In ", type_gov), color= "goldenrod")
out_area_posmatch <- list(title= paste0("Out ", type_gov), color= "olivedrab")
y_axis_title_posmatch <- "Number of Units"
x_axis_title_posmatch <- "Propensity Score"
legend_title_posmatch <- "Window Pixels"
plot_title_posmatch <- "Post Matching"

# Create post-matching plot.
posmatchversusplot <- ggplot(data = includedListposmatching[["0"]]) +
  geom_histogram(data = includedListposmatching[["1"]], aes(x = psvalue, y = ..count.., 
                                                            fill = in_area_posmatch$title, color = in_area_posmatch$title)) +
  geom_histogram(aes(x = psvalue, y = -..count.., fill = out_area_posmatch$title, color = out_area_posmatch$title)) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = alpha(c(out_area_posmatch$color, in_area_posmatch$color), 0.5), name = legend_title_posmatch) +
  scale_color_manual(values = alpha(c(out_area_posmatch$color, in_area_posmatch$color), 1), name = legend_title_posmatch) +
  labs(x = x_axis_title_posmatch, y = y_axis_title_posmatch, title = plot_title_posmatch) +
  scale_x_continuous(limits = c(0, 1)) +
  guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 10))

# Arrange pre and post matching plots together for comparison.
summ_matching_propension_plot <- ggpubr::ggarrange(plotlist = list(PreMatchversusplot, posmatchversusplot), common.legend = T, legend = "bottom")
summ_matching_propension_plot








# Analysis post-matching ####
# From this point onwards, response variables from the matched groups can be evaluated. 
# In this case, we will analyze changes per pixel in variables related to forests and carbon in 2000 and 2021

# Map the matched groups
# Retrieve matching pairs from the 'matchit' result to align the treatment (T) and control (C) groups for further analysis.
matches <- data.frame(m.nn$match.matrix)
group1 <- match(row.names(matches), row.names(y))
group2 <- match(matches[, 1], row.names(y))



# Estimate forest and carbon trends ####

########### Forests effect
# Extract forest status data for the treated T group at different time points.
forest_yT2000 <- y$Fores_2000[group1]
forest_yT2021 <- y$Fores_2021[group1]

# Extract forest status data for the control C group at different time points.
forest_yC2000 <- y$Fores_2000[group2]
forest_yC2021 <- y$Fores_2021[group2]

########### Carbon effect
carbon_yt<- y$Carbon_pixel[group1]
carbon_yC<- y$Carbon_pixel[group2] 

## summ match  
matched.cases_forest <- cbind(matches, forest_yT2000, forest_yT2021, forest_yC2000, forest_yC2021, carbon_yt, carbon_yC)

## Organize control data ####

# control_pre matching. Use the entire dataset to define the control group before matching to understand baseline conditions.
Control <- data
control_pre_forest_2020 =sum(Control$Fores_2000)
control_pre_forest_2021 =sum(Control$Fores_2021) 

# Calculate the proportion of forest pixels that remained unchanged in the control group pre-matching.
Prop_noloss_forest_control_pre= (control_pre_forest_2021/control_pre_forest_2020)*100
# Calculate the proportion of forest loss in the control group pre-matching.
Prop_forest_control_pre= 100-Prop_noloss_forest_control_pre

# Estimate confidence intervals for forest loss proportion in the control group pre-matching.
# The DescTools::BinomCI function computes confidence intervals for binomial proportions, providing a range of values within which the true proportion is likely to lie.https://rdrr.io/cran/DescTools/man/BinomCI.html
binomial_test_control_pre<- 100* (1 - DescTools::BinomCI(x= control_pre_forest_2021, n= length(Control$Fores_2000), 
                                                           conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
lower_def_control_pre<- min(binomial_test_control_pre[c("lwr.ci", "upr.ci")])
upper_def_control_pre<- max(binomial_test_control_pre[c("lwr.ci", "upr.ci")])
    
# control_pos matching. Define the control group from the data after matching analysis.
control_pos_forest_2020 = sum(matched.cases_forest$forest_yC2000)
control_pos_forest_2021 = sum(matched.cases_forest$forest_yC2021)

# Calculate the proportion of forest pixels that remained unchanged in the control group pos-matching.
Prop_noloss_forest_control_pos= (control_pos_forest_2021/control_pos_forest_2020)*100

# Calculate the proportion of forest loss in the control group pos-matching.
Prop_forest_control_pos= 100-Prop_noloss_forest_control_pos
  

# Estimate confidence intervals for forest loss proportion in the control group pos-matching. 
binomial_test_control_pos<- 100* (1 - DescTools::BinomCI(x= control_pos_forest_2021, n= length(matched.cases_forest$forest_yT2000), 
                                                         conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
lower_def_control_pos<- min(binomial_test_control_pos[c("lwr.ci", "upr.ci")])
upper_def_control_pos<- max(binomial_test_control_pos[c("lwr.ci", "upr.ci")])
  
  
## Organize treatment data ####

# Define the treatment group from the data after matching analysis.
treatment_forest_2020 = sum(matched.cases_forest$forest_yT2000)
treatment_forest_2021 = sum(matched.cases_forest$forest_yT2021) 
  
# Calculate the proportion of forest pixels that remained unchanged in the treatment group pos-matching.
Prop_noloss_forest_treatment = (treatment_forest_2021 / treatment_forest_2020) * 100

# Calculate the proportion of forest loss in the treatment group pos-matching.
Prop_forest_treatment = 100 - Prop_noloss_forest_treatment

# Estimate confidence intervals for forest loss proportion in the treatment group pos-matching. 
binomial_test_treat<- 100* (1 - DescTools::BinomCI(x= treatment_forest_2021, n= length(matched.cases_forest$forest_yT2000), 
                                                   conf.level = 0.95, method = "wilson" )) %>% as.data.frame()

# Estimate confidence intervals for forest loss proportion in the treatment group pos-matching.
lower_def_treat<- min(binomial_test_treat[c("lwr.ci", "upr.ci")])
upper_def_treat<- max(binomial_test_treat[c("lwr.ci", "upr.ci")])


  
  
## Estimation of treatment significance - governance type ####
# Model to assess changes in forest status from 2000 to 2021 as a function of governance type and selected variables.

sign_form_treatment<- as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov, "+"), paste(selected_variables, collapse= "+")))
  
# Fit a glm to the matched data to evaluate the effect of governance and other covariates on forest status.
Model_M_forest = glm( sign_form_treatment , data = y ,family = binomial)
Model_M_forest_deviance<- deviance(Model_M_forest)
Model_M_forest_AIC<- extractAIC(Model_M_forest)[2]
Model_M_forest_sum<- summary(Model_M_forest)
  
# Fit a model focused on forest changes from 2000 to 2021 as influenced by governance type alone, simplifying the influence of other variables.
formula_forest2000_2021<- as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov)))
Model_M_forest_2000_2021 = glm(formula_forest2000_2021, data = y ,family = binomial)
Model_M_forest_2000_2021_deviance<- deviance(Model_M_forest_2000_2021)
Model_M_forest_2000_2021_AIC<- extractAIC(Model_M_forest_2000_2021)[2]
Model_M_forest_2000_2021_sum <- summary(Model_M_forest_2000_2021)
sign_Model_M_forest_2000_2021_sum<- Model_M_forest_2000_2021_sum$coefficients[2,4]

# Analyze the impact of governance on the deforestation rate, examining if governance influences the likelihood of deforestation.
y<- y %>%  dplyr::mutate(deforest= dplyr::if_else(Fores_2000 ==1 & Fores_2021 == 0, 1, 0))
formula_deforest2000_2021<- as.formula(  paste0("deforest ~", type_gov) )
  Model_M_deforest_2000_2021 = glm(formula_deforest2000_2021, data = y ,family = binomial)
  Model_M_deforest_2000_2021_deviance<- deviance(Model_M_deforest_2000_2021)
  Model_M_deforest_2000_2021_AIC<- extractAIC(Model_M_deforest_2000_2021)[2]
  Model_M_deforest_2000_2021_sum <- summary(Model_M_deforest_2000_2021)
  sign_Model_M_deforest_2000_2021_sum<- Model_M_deforest_2000_2021_sum$coefficients[2,4]
  

sign_form_treatment; formula_forest2000_2021; formula_deforest2000_2021
  
# Organize forest analysis data
# This table summarizes forest data for treatment and control groups pre- and post-matching, detailing forest loss, non-loss proportions, their confidence intervals, and statistical significance
summary_forest<- data.frame(fd= c("treatment", "control_pre", "control_pos"),
                              forest_2000_2020= c(treatment_forest_2020 , control_pre_forest_2020, control_pos_forest_2020),
                              forest_2000_2021= c(treatment_forest_2021 , control_pre_forest_2021, control_pos_forest_2021),
                              forest_prop_noloss= c(Prop_noloss_forest_treatment , Prop_noloss_forest_control_pre, Prop_noloss_forest_control_pos),
                              forest_prop_loss= c(Prop_forest_treatment , Prop_forest_control_pre, Prop_forest_control_pos),
                              low_interval= c(lower_def_treat, lower_def_control_pre, lower_def_control_pos ), 
                              upper_interval= c(upper_def_treat, upper_def_control_pre, upper_def_control_pos ),
                              zval_forest_2000_2021 = c(sign_Model_M_forest_2000_2021_sum, NA, NA),
                              zval_deforest_2000_2021 = c(sign_Model_M_deforest_2000_2021_sum, NA, NA)  ) %>% 
    dplyr::mutate(type= type_gov, fd= factor(fd, levels=  c("treatment", "control_pos", "control_pre"))) %>% 
      dplyr::mutate(sign_forest_2000_2021= sapply(.$zval_forest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"} else {""} })  ) %>% 
    dplyr::mutate(sign_deforest_2000_2021= sapply(.$zval_deforest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"}  else {""} } )  )
  

# Plotting forest estimations
# Define the colors and labels for the plotting
guide_fill_forest<- list(
  data.frame(fd= "control_pre", label_fill= "Control preMatching", color_fill= "lightskyblue1"),
  data.frame(fd= "control_pos", label_fill = "Control posMatching", color_fill= "rosybrown1"),
  data.frame(fd= "treatment", label_fill = "Treatment", color_fill = "lightgoldenrodyellow")
) %>% rbind.fill()

guide_xaxis_forest <- list(
  data.frame(fd= "control_pre", label_x= "Control preMatching" ),
  data.frame(fd= "control_pos", label_x = "Control posMatching" ),
  data.frame(fd= "treatment", label_x = "Treatment" )
) %>% rbind.fill()

y_axis_title_result_forest<- "Forest loss (%)"
x_axis_title_result_forest<- type_gov
legend_title_result_forest<- ""
plot_title_result_forest<- paste("Forests", type_gov)

dataplot_forest<- summary_forest %>%
  dplyr::mutate(fd= factor( fd, levels = unique(.$fd) )) %>% 
  list(guide_fill_forest, guide_xaxis_forest) %>% plyr::join_all() %>% 
  dplyr::mutate(
    label_fill= factor(label_fill, unique(guide_fill_forest$label_fill)),
    label_x= factor(label_x, levels= unique(guide_xaxis_forest$label_x))
  )

# plot the proportion of forest loss 
plot_forest<- ggplot(data= dataplot_forest,  aes(x= label_x, y= forest_prop_loss , fill= label_fill))+
  geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
  geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                width = 0.1, position =  position_dodge(width  = .8), color = "black")+
  xlab(x_axis_title_result_forest)+ylab(y_axis_title_result_forest)+
  scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(legend_title_result_forest,  values = setNames(guide_fill_forest$color_fill ,guide_fill_forest$label_fill) )+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x  = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"))

# plot the proportion of forest loss  with significance annotations
plot_forest_sign <- ggplot(data= dataplot_forest,  aes(x= label_x, y= forest_prop_loss , fill= label_fill))+
  geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
  geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                width = 0.1, position =  position_dodge(width  = .8), color = "black")+
  geom_signif(position="identity",  textsize = 5, comparisons=list(c("Control posMatching","Treatment")) , annotations = dataplot_forest$sign_forest_2000_2021[1] )+
  xlab(x_axis_title_result_forest)+ylab(y_axis_title_result_forest)+
  scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(legend_title_result_forest,  values = setNames(guide_fill_forest$color_fill ,guide_fill_forest$label_fill) )+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x  = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"))








  
    
                  
  
########### Carbon effect

# Pre-matching control
control_pre_carbon_2000 = dplyr::filter(Control, Fores_2000 == 1)$Carbon_pixel
control_pre_carbon_2021 = dplyr::filter(Control, Fores_2021 == 1)$Carbon_pixel
  
# Identify carbon pixel data associated with deforestation from 2000 to 2021 from original data
loss_pre_carbon<- dplyr::filter(Control, Fores_2000 == 1 & Fores_2021 == 0 )$Carbon_pixel
  
# Calculate metrics about carbon in deforestation pixels.
mean_pre_carbon<- mean(loss_pre_carbon)
sum_pre_carbon<- sum(loss_pre_carbon)

sum_control_pre_carbon_2000<- sum(control_pre_carbon_2000, na.rm=T)
sum_control_pre_carbon_2021<- sum(control_pre_carbon_2021, na.rm=T)

mean_control_pre_carbon_2000<- mean(control_pre_carbon_2000, na.rm=T)
mean_control_pre_carbon_2021<- mean(control_pre_carbon_2021, na.rm=T)
  
  
# Calculate the proportion of carbon retained and lost in pixels with forests before matching.
Prop_noloss_carbon_control_pre<- (sum_control_pre_carbon_2021/sum_control_pre_carbon_2000)*100
Prop_carbon_control_pre<- 100-Prop_noloss_carbon_control_pre
  
# Estimate confidence intervals for carbon loss proportion in the control group pre-matching.
binomial_test_losscarb_control_pre<- 100* (1 - DescTools::BinomCI(x= sum_control_pre_carbon_2021, n= sum(control_pre_carbon_2000, na.rm=T), 
                                                           conf.level = 0.95, method = "wilson" )) %>% as.data.frame()

lower_losscarb_control_pre<- min(binomial_test_losscarb_control_pre[c("lwr.ci", "upr.ci")])
upper_losscarb_control_pre<- max(binomial_test_losscarb_control_pre[c("lwr.ci", "upr.ci")])

# Organize data
dispersion_pre_carbon_effect <- list(t1= control_pre_carbon_2000, t2= control_pre_carbon_2021, loss= loss_pre_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "control_pre")
  
  

##  Pos-matching control
  control_pos_carbon_2000 = dplyr::filter(matched.cases_forest, forest_yC2000 == 1)$carbon_yC
  control_pos_carbon_2021 = dplyr::filter(matched.cases_forest, forest_yC2021 == 1)$carbon_yC

# Identify carbon pixel data associated with deforestation from 2000 to 2021 from matching data
loss_pos_carbon<- dplyr::filter(matched.cases_forest, forest_yC2000 == 1 & forest_yC2021 == 0 )$carbon_yt
  

# Calculate metrics about carbon in deforestation pixels.
  mean_pos_carbon<- mean(loss_pos_carbon)
  sum_pos_carbon<- sum(loss_pos_carbon)

  sum_control_pos_carbon_2000<- sum(control_pos_carbon_2000, na.rm=T)
  sum_control_pos_carbon_2021<- sum(control_pos_carbon_2021, na.rm=T)
  
  mean_control_pos_carbon_2000<- mean(control_pos_carbon_2000, na.rm=T)
  mean_control_pos_carbon_2021<- mean(control_pos_carbon_2021, na.rm=T)
  
  # Calculate the proportion of carbon retained and lost in pixels with forests after matching.
  Prop_noloss_carbon_control_pos<- (sum_control_pos_carbon_2021/sum_control_pos_carbon_2000)*100
  Prop_carbon_control_pos<- 100-Prop_noloss_carbon_control_pos
  
  
  # Estimate confidence intervals for carbon loss proportion in the control group pos-matching.
  binomial_test_losscarb_control_pos<- 100* (1 - DescTools::BinomCI(x= sum_control_pos_carbon_2021, n= sum(control_pos_carbon_2000, na.rm=T), 
                                                                    conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
  lower_losscarb_control_pos<- min(binomial_test_losscarb_control_pos[c("lwr.ci", "upr.ci")])
  upper_losscarb_control_pos<- max(binomial_test_losscarb_control_pos[c("lwr.ci", "upr.ci")])
  
  # Organize data
  dispersion_pos_carbon_effect <- list(t1= control_pos_carbon_2000, t2= control_pos_carbon_2021, loss= loss_pos_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "control_pos")
  
  
##  Pos-matching treatment
  treatment_pos_carbon_2000 = dplyr::filter(matched.cases_forest, forest_yT2000 == 1)$carbon_yt
  treatment_pos_carbon_2021 = dplyr::filter(matched.cases_forest, forest_yT2021 == 1)$carbon_yt
  
  # Calculate metrics about carbon in deforestation pixels.
  mean_pos_carbon<- mean(loss_pos_carbon)
  sum_pos_carbon<- sum(loss_pos_carbon)
  
  sum_treatment_pos_carbon_2000<- sum(treatment_pos_carbon_2000, na.rm=T)
  sum_treatment_pos_carbon_2021<- sum(treatment_pos_carbon_2021, na.rm=T)
  
  mean_treatment_pos_carbon_2000<- mean(treatment_pos_carbon_2000, na.rm=T)
  mean_treatment_pos_carbon_2021<- mean(treatment_pos_carbon_2021, na.rm=T)

  
  loss_treatment_pos_carbon<- dplyr::filter(matched.cases_forest, forest_yT2000 == 1 & forest_yT2021 == 0 )$carbon_yt
  mean_treatment_pos_carbon<- mean(loss_treatment_pos_carbon)
  sum_treatment_pos_carbon<- sum(loss_treatment_pos_carbon)
  
  # Calculate the proportion of carbon retained and lost in pixels with forests after matching.
  Prop_noloss_carbon_treatment_pos<- (sum_treatment_pos_carbon_2021/sum_treatment_pos_carbon_2000)*100
  Prop_carbon_treatment_pos<- 100-Prop_noloss_carbon_treatment_pos
  
  
  # Estimate confidence intervals for carbon loss proportion in the treatment group pos-matching.
  binomial_test_losscarb_treatment_pos<- 100* (1 - DescTools::BinomCI(x= sum_treatment_pos_carbon_2021, n= sum(treatment_pos_carbon_2000, na.rm=T), 
                                                                    conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
  lower_losscarb_treatment_pos<- min(binomial_test_losscarb_treatment_pos[c("lwr.ci", "upr.ci")])
  upper_losscarb_treatment_pos<- max(binomial_test_losscarb_treatment_pos[c("lwr.ci", "upr.ci")])
  
  # Organize data
  dispersion_carbon_effect <- list(t1= treatment_pos_carbon_2000, t2= treatment_pos_carbon_2021, loss= loss_pos_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "treatment_pos")

  # Organize carbon analysis data
  # This table summarizes carbon data for treatment and control groups pre- and post-matching, detailing carbon loss, non-loss proportions, their confidence intervals, and statistical significance. It provides an overview of carbon dynamics in response to forest management and deforestation across different periods and groups.
  summary_carbon<- data.frame(fd= c("treatment", "control_pos", "control_pre"),
                              sum_carbon_t1= c(sum_treatment_pos_carbon_2000, sum_control_pos_carbon_2000 , sum_control_pre_carbon_2000),
                              sum_carbon_t2= c(sum_treatment_pos_carbon_2021, sum_control_pos_carbon_2021 , sum_control_pre_carbon_2021),
                              sum_carbon= c(sum_treatment_pos_carbon, sum_pos_carbon, sum_pre_carbon),
                              sum_carbon_prop_noloss= c(Prop_noloss_carbon_treatment_pos, Prop_noloss_carbon_control_pos , Prop_noloss_carbon_control_pre),
                              sum_carbon_prop_loss= c(Prop_carbon_treatment_pos, Prop_carbon_control_pos , Prop_carbon_control_pre),
                              mean_carbon_t1= c(mean_treatment_pos_carbon_2000, mean_control_pos_carbon_2000 , mean_control_pre_carbon_2000),
                              mean_carbon_t2= c(mean_treatment_pos_carbon_2021, mean_control_pos_carbon_2021 , mean_control_pre_carbon_2021),
                              mean_pixel = c(mean_treatment_pos_carbon, mean_pos_carbon, mean_pre_carbon),
                              low_interval= c(lower_losscarb_treatment_pos, lower_losscarb_control_pos, lower_losscarb_control_pre), 
                              upper_interval= c(upper_losscarb_treatment_pos, upper_losscarb_control_pos, upper_losscarb_control_pre),
                              zval_forest_2000_2021 = c(sign_Model_M_forest_2000_2021_sum, NA, NA),
                              zval_deforest_2000_2021 = c(sign_Model_M_deforest_2000_2021_sum, NA, NA)  ) %>%                         
    dplyr::mutate(type= type_gov, fd= factor(fd, levels=  c("treatment", "control_pos", "control_pre"))) %>% 
    dplyr::mutate(sign_forest_2000_2021= sapply(.$zval_forest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"} else {""} })  ) %>% 
    dplyr::mutate(sign_deforest_2000_2021= sapply(.$zval_deforest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"}  else {""} } )  )
  
  # Plotting carbon estimations
  # Define the colors and labels for the plotting
  guide_fill_carbon<- list(
    data.frame(fd= "control_pre", label_fill= "Control preMatching", color_fill= "lightskyblue1"),
    data.frame(fd= "control_pos", label_fill = "Control posMatching", color_fill= "rosybrown1"),
    data.frame(fd= "treatment", label_fill = "Treatment", color_fill = "lightgoldenrodyellow")
  ) %>% rbind.fill()
  
  guide_xaxis_carbon <- list(
    data.frame(fd= "control_pre", label_x= "Control preMatching"),
    data.frame(fd= "control_pos", label_x= "Control posMatching"),
    data.frame(fd= "treatment", label_x = "Treatment")
  ) %>% rbind.fill()
  
  y_axis_title_result_carbon<- "Proportion of Carbon loss (%)"
  x_axis_title_result_carbon<- type_gov
  legend_title_result_carbon<- ""
  plot_title_result_carbon<- paste("Carbon", type_gov)
  
  dataplot_carbon<- summary_carbon %>%
    dplyr::mutate(fd= factor( fd, levels = unique(.$fd) )) %>% 
    list(guide_fill_carbon, guide_xaxis_carbon) %>% plyr::join_all() %>% 
    dplyr::mutate( label_fill= factor(label_fill, unique(guide_fill_carbon$label_fill)),
                   label_x= factor(label_x, levels= unique(guide_xaxis_carbon$label_x)) )
  
  
  
  
  
  
  # plot the proportion of carbon loss 
  plot_loss_carbon<- ggplot(data= dataplot_carbon,  aes(x= type, y= sum_carbon_prop_loss , fill= label_fill))+
    geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
    geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                  width = 0.05, position =  position_dodge(width  = .8), color = "black")+
    xlab(x_axis_title_result_carbon)+
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%"))+
    scale_fill_manual(legend_title_result_carbon, 
                      values = setNames(guide_fill_carbon$color_fill ,
                                        guide_fill_carbon$label_fill) )+
    theme_minimal()+ylab(y_axis_title_result_carbon)+
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(angle = 90, hjust = 1),
      axis.line.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"))
  
  
  plot_loss_carbon_fin<- ggplot(data= dataplot_carbon,  aes(x= label_x, y= sum_carbon_prop_loss , fill= label_fill))+
    geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
    geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                  width = 0.1, position =  position_dodge(width  = .8), color = "black")+
    # geom_signif(position="identity",  textsize = 5, comparisons=list(c("Control posMatching","Treatment")) ,annotations = dataplot_carbon$sign_forest_2000_2021[1])+
    xlab(x_axis_title_result_carbon)+ylab(y_axis_title_result_carbon)+
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
    scale_fill_manual(legend_title_result_carbon,  values = setNames(guide_fill_carbon$color_fill ,guide_fill_carbon$label_fill) )+
    theme_minimal()+
    theme(legend.position = "bottom",
          axis.text.x  = element_blank(),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))
  
  # plot the proportion of carbon loss  with significance annotations
  plot_loss_carbon_sign <- ggplot(data= dataplot_carbon,  aes(x= label_x, y= sum_carbon_prop_loss , fill= label_fill))+
    geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
    geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                  width = 0.1, position =  position_dodge(width  = .8), color = "black")+
    geom_signif(position="identity",  textsize = 5, comparisons=list(c("Control posMatching","Treatment")) ,annotations = dataplot_carbon$sign_forest_2000_2021[1])+
    xlab(x_axis_title_result_carbon)+ylab(y_axis_title_result_carbon)+
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
    scale_fill_manual(legend_title_result_carbon,  values = setNames(guide_fill_carbon$color_fill ,guide_fill_carbon$label_fill) )+
    theme_minimal()+
    theme(legend.position = "bottom",
          axis.text.x  = element_blank(),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))
  
  plot_loss_carbon_sign
  
  
  # Arrange the forest and carbon loss plots into a single figure
  plot_response_sign <- ggpubr::ggarrange(
    plotlist = list(plot_forest_sign, plot_loss_carbon_sign),  # List of plots to arrange
    common.legend = TRUE,  # Use a single common legend for all plots
    legend = "bottom"  # Position the legend at the bottom of the arranged figure
  )
  
  plot_response_sign
  

##  Plotting carbon disperssion estimations
  # Define the colors and labels for the plotting
  
  guide_fill_carbon_disperssion<- list(
    data.frame(treatment= "control_pre", label_fill= "Control preMatching", color_fill= "lightskyblue1"),
    data.frame(treatment= "control_pos", label_fill = "Control posMatching", color_fill= "rosybrown1"),
    data.frame(treatment= "treatment_pos", label_fill = "Treatment", color_fill = "lightgoldenrodyellow")
    ) %>% rbind.fill()
  
  guide_xaxis_carbon_disperssion<- list(
    data.frame(level= "t1", label_x= "Forests 2000"),
    data.frame(level= "t2", label_x= "Forests 2021"),
    data.frame(level= "loss", label_x = "Forest 2021- Forest 2000")) %>% 
    rbind.fill()
  
  y_axis_title_result_carbon_disperssion<- "Carbon loss by pixel"
  x_axis_title_result_carbon_disperssion<- "Forest Over Time"
  legend_title_result_carbon_disperssion<- ""
  plot_title_result_carbon_disperssion<- paste("Carbon ", type_gov)
  limits_axis_y<- boxplot.stats(dataplot_carbon_disperssion$value)$stats %>% {c(min(.), max(.))}
  
  # Organize data
  dataplot_carbon_disperssion <- list(dispersion_pre_carbon_effect, 
                                      dispersion_pos_carbon_effect, 
                                      dispersion_carbon_effect) %>%
    rbind.fill() %>% dplyr::mutate(treatment= factor( treatment, levels = unique(.$treatment) )) %>% 
    list(guide_fill_carbon_disperssion, guide_xaxis_carbon_disperssion) %>% plyr::join_all() %>% 
    dplyr::mutate(label_x= factor(label_x, unique(guide_xaxis_carbon_disperssion$label_x)),
                  label_fill= factor(label_fill, unique(guide_fill_carbon_disperssion$label_fill))
                  )
  
  dataplot_carbon_disperssion_t1_t2<- dplyr::filter(dataplot_carbon_disperssion, !level %in% "loss")
  dataplot_carbon_disperssion_loss<- dplyr::filter(dataplot_carbon_disperssion, level %in% "loss")
  
  # plot the disperssion of carbon by pixel between two periods
  plot_carbon_disperssion_t1_t2<- ggplot(dataplot_carbon_disperssion_t1_t2, aes(x = label_x, y = value, fill= label_fill)) +
    stat_slab(side = "right", scale = 0.4,show.legend = T,expand = F, 
              position = position_dodge(width = .8), aes(fill_ramp = stat(level) ), 
              .width = c(.50, .95)  ) +
    geom_boxplot(width = 0.2, outlier.alpha=0, size= 0.1, 
                 position = position_dodge(width  = .8),show.legend = FALSE)+
    scale_fill_manual(legend_title_result_carbon_disperssion, values = setNames(guide_fill_carbon_disperssion$color_fill,
                                                                    guide_fill_carbon_disperssion$label_fill) )+
    guides(fill_ramp = "none")  +
    scale_y_continuous(limits= limits_axis_y)+
    labs(x = x_axis_title_result_carbon_disperssion, y = y_axis_title_result_carbon_disperssion)+
    theme_minimal()+
    theme(
      axis.text.y = element_text(angle = 90, hjust = 1),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))

  # plot for carbon loss dispersion pixels
  plot_carbon_disperssion_loss<- ggplot(dataplot_carbon_disperssion_loss, aes(x = label_x, y = value, fill= label_fill)) +
    stat_slab(side = "right", scale = 0.4,show.legend = T,expand = F, 
              position = position_dodge(width = .8), aes(fill_ramp = stat(level) ), 
              .width = c(.50, .95)  ) +
    geom_boxplot(width = 0.2, outlier.alpha=0, size= 0.1, 
                 position = position_dodge(width  = .8),show.legend = FALSE)+
    scale_fill_manual(legend_title_result_carbon_disperssion, values = setNames(guide_fill_carbon_disperssion$color_fill,
                                                                    guide_fill_carbon_disperssion$label_fill)
                      )+
    guides(fill_ramp = "none")  +
    scale_y_continuous(limits= limits_axis_y)+
    labs(x = "Carbon loss", y = " ")+
    theme_minimal()+
    theme(
      axis.text.y = element_text(angle = 90, hjust = 1),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))
  
  
  # Display the final compiled disperssion carbon plot
  plot_carbon_final<- ggarrange(plotlist = list(plot_carbon, plot_loss_carbon,
                                             plot_carbon_disperssion_t1_t2, 
                                             plot_carbon_disperssion_loss),
                                      common.legend = T, legend = "bottom", nrow = 2, ncol=2 )

  plot_carbon_final
  
  
  