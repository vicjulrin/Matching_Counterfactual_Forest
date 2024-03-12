# Listar paquetes necesarios
packages_list<-list("magrittr", "dplyr", "plyr", "MatchIt", "RItools", "Hmisc", "this.path", "scales", "ggdendro", "data.table", "openxlsx",
                    "tibble", "leaps", "pbapply", "RColorBrewer", "ggpubr", "ggdist")

# Instalar paquetes necesarios no instalados
packagesPrev<- .packages(all.available = TRUE)
lapply(packages_list, function(x) {   if ( ! x %in% packagesPrev ) { install.packages(x, force=T)}    })

# Cargar librerias
lapply(packages_list, library, character.only = TRUE)

# Establecer directorio de trabajo
dir_work<- this.path::this.path() %>% dirname() # espacio de trabajo donde se almacena el codigo actual

# Establecer folder input - output
input<- file.path(dirname(dir_work), "input")
output<- file.path(dirname(dir_work), "output")


## Execute script

### Revisar como se generaron, y modificarlos si es el caso

# matching using propensity score
setwd(input)
data<- read.csv("data.csv", header = T)
type_gov<- "Type"

# List covars
covars<-c( "Department", "Anual_Prec", "Prec_Seas",  "Dis_Def",    "Dis_Rivers",
          "District_R", "Departme_R", "National_R", "D7Set10",    "D7Set1000",  "D7Set5000",  "D7Set10000", "D17Set10",   "D17Set1000", "D17Set5000",
          "D17Set10000", "Ecoregions", "Elevation",  "Pop2000",    "Pop2020",    "Slope",      "Tra_Time00", "Tra_Time15")

  
  # Check coeffcients forests ~ governanza type
  formula_test_varsForest<- as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov, "+"), paste(covars, collapse= "+")))
  test_varsForest<- glm(formula_test_varsForest, data = data, family = binomial()) 
  
  summ_varsForest<- summary(test_varsForest)
  
  cofs_varsForest <- data.frame(Coefficient = rownames(summ_varsForest$coefficients), Value = summ_varsForest$coefficients) %>% 
    dplyr::filter(!Coefficient %in% "(Intercept)") %>% 
    dplyr::mutate(p_val= round(Value.Pr...z.., 3)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(sign= if(Value.Pr...z..<0.001){"***"}else if (Value.Pr...z..<0.001) {"**"} else if(Value.Pr...z..<0.05) {"*"} else {""} ) %>% 
    dplyr::mutate(sign_label= paste(p_val, sign), min_std= Value.Estimate-Value.Std..Error, max_std= Value.Estimate+Value.Std..Error,
                  type_gov= type_gov)  
  
  # Remove high correlated variables
  cor_threshold<- 0.65 # define correlation threshold
  
  formula_test_multicor<- as.formula( paste0(type_gov, "~", paste0(covars, collapse = "+")) )
  test_multicor<- glm(formula_test_multicor, data = data, family = binomial()) # sort by inflacion de varianza
  vif_data<- car::vif(test_multicor) %>% as.data.frame() %>% {data.frame(Var= rownames(.), VIF= .[,1])} %>% arrange(VIF)
  
  vif_values <-  vif_data %>% 
    dplyr::mutate(Variable1= Var, VIF_var1= VIF, Variable2= Var, VIF_var2= VIF) %>%  dplyr::arrange("VIF")
  
  cordataR<- summary(test_multicor, correlation=T)[["correlation"]] %>% as.data.frame.matrix()
  cordataR[,"(Intercept)"]<- NULL; cordataR<- cordataR[2:nrow(cordataR), ]# ELIMINAR INTERCEPT MATRIZ DE CORRELACION
  
  
  NACol<- names(which(rowSums(is.na(cordataR)) > (ncol(cordataR)/2) ))
  cordata<- cordataR %>% {.[!names(.) %in% NACol,]} %>% {.[,!colnames(.) %in% NACol]}
  cordata[is.na(cordata)]<-0
  
  corhclust <- hclust(as.dist(1-abs(cordata))) 
  cordend<-as.dendrogram(corhclust)
  cordend_data <- dendro_data(cordend)
  
  rank_covars<- cutree(corhclust, h = 1-cor_threshold) %>% as.data.frame %>% rownames_to_column("Var") %>% setnames(".", "group") %>%
      dplyr::filter(!Var %in% "(Intercept)") %>% list(vif_data) %>% join_all() %>% arrange(group, VIF)
    
  covars_no_multicol<- dplyr::filter(rank_covars, !duplicated(group))$Var

  var_table <- with(cordend_data$labels, data.frame(y_center = x, y_min= x-0.5, y_max=x+0.5, Variable = as.character(label), height = 1))
  col1<- "#EBEBEB"; col2<- "white"; is.even<- function(x){ x %% 2 == 0 }
  var_table$col<- rep_len(c(col1, col2), length.out=length(var_table$Variable)) %>% {if(is.even(length(.))) {rev(.)} else {.}}
  segment_data <- with(segment(cordend_data), data.frame(x = y, y = x, xend = yend, yend = xend, cor= 1-yend))

  # Check better model
  pre_formula_glm<- as.formula( paste0(type_gov, "~", paste0(covars_no_multicol, collapse = "+")) )
  
  # Estimar modelos forward y backward
  model  <- regsubsets(pre_formula_glm, data = data, nvmax = length(covars), method = "seqrep")
  summ_model<-summary(model)[c("rsq", "rss", "adjr2", "cp", "bic" )] %>% as.data.frame() %>% dplyr::mutate(model= seq(nrow(.)))
  

  # Estimar AICs
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
  forms_models<- rbind.fill(purrr::map(AIC_models, "data_form"))
    
  better_models<- rbind.fill(purrr::map(AIC_models, "data_AIC")) %>% list(summ_model) %>% join_all() %>% 
    dplyr::arrange( bic) %>% dplyr::mutate(rank_BIC= seq(nrow(.))) %>% 
    dplyr::arrange(AIC) %>% dplyr::mutate(rank_AIC= seq(nrow(.)))
  
  data_vars<-  rbind.fill(purrr::map(AIC_models, "data_vars")) %>% list(better_models) %>% 
    join_all() %>%  dplyr::group_by(vars) %>% dplyr::mutate(freq_var= n()) %>% 
    dplyr::arrange(freq_var) %>% dplyr::mutate(vars= factor(vars, levels = unique(.$vars)) ) 
  

  # Seleccion del mejor modelo
  View(better_models) #### revision investigador de reesultados
  critera<- "AIC" # nombrar el criterio con el cual seleccionar el mejor modelo; aic POR DEFECTO

  vars_models<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
    dplyr::mutate(model= factor(model, levels = unique(.$model)) ) %>% as.data.frame()
  
  better_model<- unique(vars_models[1,1])
  better_model
  
  # selected variables
  selected_variables<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
    dplyr::filter(model %in% better_model) %>% {as.character(.$vars)}
  
  # Matching Analysis
  
  # (1)
  treated <-(data[,type_gov] ==1) 
  cov <-data[,selected_variables]
  std.diff <-apply(cov,2,function(x) 100*(mean(x[treated])- mean(x[!treated]))/(sqrt(0.5*(var(x[treated]) + var(x[!treated]))))) %>% abs()
  
  
  # (3)
  formula_glm<- as.formula( paste0(type_gov, "~", paste0(selected_variables, collapse = "+")) )
  ps <- glm(formula_glm, data = data, family = binomial())
  
  # (4)
  data$psvalue <- predict(ps, type = "response")
  
  # (5)
  m.nn <- matchit(formula_glm, data =data, method= "nearest", ratio = 1)

  
  
  
  # (6)
  y=match.data(m.nn, group="all") %>% dplyr::mutate(deforest= dplyr::if_else(Fores_2000 ==1 & Fores_2021 == 0, 1, 0))
  treated1 <-(y[, type_gov]==1)
  cov1 <-y[, selected_variables]
  
  std.diff1 <-apply(cov1,2,function(x) 100*(mean(x[treated1])- mean(x[!treated1]))/(sqrt(0.5*(var(x[treated1]) + var(x[!treated1]))))) 
  
  # estimate percent balance improvement
  balance_improvement<- list(rownames_to_column(as.data.frame(std.diff), "variable"),
                             rownames_to_column(as.data.frame(std.diff1), "variable")) %>% join_all() %>%
    mutate_at(vars(std.diff, std.diff1), abs) %>% 
    dplyr::mutate(improve_percent= ((std.diff - std.diff1) / std.diff) * 100  )
  

  # create graham S2 table
  summ_match<- summary(m.nn)

  table_graham_pre<- summ_match[["sum.all"]] %>% as.data.frame()  %>% {.[2:nrow(.),]} %>% 
    dplyr::mutate(type= type_gov, time= "before") %>% tibble::rownames_to_column("variable") %>% 
    dplyr::relocate( "type", "time", "variable", .before = 1)

  table_graham_pos<-  summary(m.nn)[["sum.matched"]] %>% as.data.frame()  %>% {.[2:nrow(.),]} %>% 
    dplyr::mutate(type= type_gov, time= "after") %>% tibble::rownames_to_column("variable") %>% 
    dplyr::relocate( "type", "time", "variable", .before = 1)
  
  table_graham_S2<- list(table_graham_pre, table_graham_pos) %>% rbind.fill() %>% arrange(variable, type) %>% 
    list(balance_improvement) %>% join_all()
  

  
  # (8)
  matches <- data.frame(m.nn$match.matrix)
  
  group1 <- match(row.names(matches), row.names(y))
  group2 <- match(matches[,1], row.names(y))
  
  ########### Forests effect
  forest_yT2000 <- y$Fores_2000[group1]
  forest_yT2021 <- y$Fores_2021[group1]
  forest_yC2000 <- y$Fores_2000[group2]
  forest_yC2021 <- y$Fores_2021[group2]
  
  ########### Carbon effect
  carbon_yt<- y$Carbon_pixel[group1]
  carbon_yC<- y$Carbon_pixel[group2] 
  
  ## summ match  
  matched.cases_forest <- cbind(matches, forest_yT2000, forest_yT2021, forest_yC2000, forest_yC2021, carbon_yt, carbon_yC)

  
  # control_pre matching
  control_pre_forest_2020 =sum(Control$Fores_2000)
  control_pre_forest_2021 =sum(Control$Fores_2021) 
  Prop_noloss_forest_control_pre= (control_pre_forest_2021/control_pre_forest_2020)*100
  Prop_forest_control_pre= 100-Prop_noloss_forest_control_pre
  
  n_control_pre <- length(Control$Fores_2000)
  prop_def_control_pre <- Prop_forest_control_pre/100

  binomial_test_control_pre<- 100* (1 - DescTools::BinomCI(x= control_pre_forest_2021, n= length(Control$Fores_2000), 
                                                           conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
  lower_def_control_pre<- min(binomial_test_control_pre[c("lwr.ci", "upr.ci")])
  upper_def_control_pre<- max(binomial_test_control_pre[c("lwr.ci", "upr.ci")])
    
  
  
  # control_pos matching
  control_pos_forest_2020 = sum(matched.cases_forest$forest_yC2000)
  control_pos_forest_2021 = sum(matched.cases_forest$forest_yC2021)
  Prop_noloss_forest_control_pos= (control_pos_forest_2021/control_pos_forest_2020)*100
  Prop_forest_control_pos= 100-Prop_noloss_forest_control_pos
  
  n_control_pos <- length(matched.cases_forest$forest_yC2000)
  prop_def_control_pos <- Prop_forest_control_pos/100

  binomial_test_control_pos<- 100* (1 - DescTools::BinomCI(x= control_pos_forest_2021, n= length(matched.cases_forest$forest_yT2000), 
                                                     conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
  lower_def_control_pos<- min(binomial_test_control_pos[c("lwr.ci", "upr.ci")])
  upper_def_control_pos<- max(binomial_test_control_pos[c("lwr.ci", "upr.ci")])
  
  
  
  
  
  
  
  
  # treatment
  treatment_forest_2020 =sum(matched.cases_forest$forest_yT2000)
  treatment_forest_2021 =sum(matched.cases_forest$forest_yT2021) 
  Prop_noloss_forest_treatment= (treatment_forest_2021/treatment_forest_2020)*100
  Prop_forest_treatment= 100-Prop_noloss_forest_treatment

  
  # significancia:  test  confidence intervals for binomial proportions  https://rdrr.io/cran/DescTools/man/BinomCI.html
  n_treat <- length(matched.cases_forest$forest_yT2000)
  prop_def_treat <- Prop_forest_treatment/100
  
  binomial_test_treat<- 100* (1 - DescTools::BinomCI(x= treatment_forest_2021, n= length(matched.cases_forest$forest_yT2000), 
                                          conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
  lower_def_treat<- min(binomial_test_treat[c("lwr.ci", "upr.ci")])
  upper_def_treat<- max(binomial_test_treat[c("lwr.ci", "upr.ci")])


  
  
  
  
  
  
  #### sign -
  sign_form_treatment<- as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov, "+"), paste(selected_variables, collapse= "+")))
  
  
  # (9) Test Significancia
  # Model after Matching
  Model_M_forest = glm( sign_form_treatment , data = y ,family = binomial)
  Model_M_forest_deviance<- deviance(Model_M_forest)
  Model_M_forest_AIC<- extractAIC(Model_M_forest)[2]
  Model_M_forest_sum<- summary(Model_M_forest)
  
  
  # (10) Test Significancia tratamiento
  
  # forest 2020_2021
  formula_forest2000_2021<- as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov)))
  Model_M_forest_2000_2021 = glm(formula_forest2000_2021, data = y ,family = binomial)
  Model_M_forest_2000_2021_deviance<- deviance(Model_M_forest_2000_2021)
  Model_M_forest_2000_2021_AIC<- extractAIC(Model_M_forest_2000_2021)[2]
  Model_M_forest_2000_2021_sum <- summary(Model_M_forest_2000_2021)
  sign_Model_M_forest_2000_2021_sum<- Model_M_forest_2000_2021_sum$coefficients[2,4]

  # deforest 2020_2021
  formula_deforest2000_2021<- as.formula(  paste0("deforest ~", type_gov) )
  Model_M_deforest_2000_2021 = glm(formula_deforest2000_2021, data = y ,family = binomial)
  Model_M_deforest_2000_2021_deviance<- deviance(Model_M_deforest_2000_2021)
  Model_M_deforest_2000_2021_AIC<- extractAIC(Model_M_deforest_2000_2021)[2]
  Model_M_deforest_2000_2021_sum <- summary(Model_M_deforest_2000_2021)
  sign_Model_M_deforest_2000_2021_sum<- Model_M_deforest_2000_2021_sum$coefficients[2,4]
  
  
  
  
  
  
  
  
  
  #### S2 Schleicher 
  table_Model_M_forest_sum<- Model_M_forest_sum[["coefficients"]] %>% as.data.frame() %>%  rownames_to_column("predictor") %>% 
    dplyr::mutate(SE= .$`Std. Error` , p= .[, ncol(.)] ) %>% dplyr::rowwise() %>% 
    dplyr::mutate(sign= if(p<0.05){"*"}else if (p<0.01) {"**"} else if(p<0.001) {"***"} else {""} ) %>% 
    dplyr::select(c("predictor", "SE", "Estimate", "p", "sign")) %>%  as.data.frame()
  
  loss_vars<- pblapply(selected_variables, function(x) {
    no_var<- selected_variables %>% {.[!. %in% x]}
    form_test_var<-  as.formula(paste0("cbind(Fores_2000,Fores_2021)", paste0("~", type_gov, "+"), paste(no_var, collapse= "+")))
    Model_test_var<- glm( form_test_var , data = y ,family = binomial)
    deviance_test_var <- deviance(Model_test_var)
    AIC_test_var<- extractAIC(Model_test_var)[2]
    
    summ_loss<- data.frame(predictor=x, loss_D= abs(diff(c(Model_M_forest_2000_2021_AIC, AIC_test_var))), 
                           loss_AIC= abs(diff(c(Model_M_forest_2000_2021_deviance, deviance_test_var))))
    
  }) %>% rbind.fill()
  
  table_M_forest_schleicher_S2<- list(table_Model_M_forest_sum, loss_vars) %>% join_all()
  
  
  
  
  
  
  
  
  
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
  
  
    
    
  
    
                  
  
  ########### Carbon effect
  # control_pre matching
  
  control_pre_carbon_2000 = dplyr::filter(Control, Fores_2000 == 1)$Carbon_pixel
  control_pre_carbon_2021 = dplyr::filter(Control, Fores_2021 == 1)$Carbon_pixel
  
  loss_pre_carbon<- dplyr::filter(Control, Fores_2000 == 1 & Fores_2021 == 0 )$Carbon_pixel
  
  mean_pre_carbon<- mean(loss_pre_carbon)
  sum_pre_carbon<- sum(loss_pre_carbon)
  
  
  sum_control_pre_carbon_2000<- sum(control_pre_carbon_2000, na.rm=T)
  sum_control_pre_carbon_2021<- sum(control_pre_carbon_2021, na.rm=T)
  
  mean_control_pre_carbon_2000<- mean(control_pre_carbon_2000, na.rm=T)
  mean_control_pre_carbon_2021<- mean(control_pre_carbon_2021, na.rm=T)
  
  Prop_noloss_carbon_control_pre<- (sum_control_pre_carbon_2021/sum_control_pre_carbon_2000)*100
  Prop_carbon_control_pre<- 100-Prop_noloss_carbon_control_pre
  
  
  binomial_test_losscarb_control_pre<- 100* (1 - DescTools::BinomCI(x= sum_control_pre_carbon_2021, n= sum(control_pre_carbon_2000, na.rm=T), 
                                                           conf.level = 0.95, method = "wilson" )) %>% as.data.frame()
  
 lower_losscarb_control_pre<- min(binomial_test_losscarb_control_pre[c("lwr.ci", "upr.ci")])
 upper_losscarb_control_pre<- max(binomial_test_losscarb_control_pre[c("lwr.ci", "upr.ci")])
  
  
  
  
  
  
  dispersion_pre_carbon_effect <- list(t1= control_pre_carbon_2000, t2= control_pre_carbon_2021, loss= loss_pre_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "control_pre")
  
  
  
  # control_pos matching
  control_pos_carbon_2000 = dplyr::filter(matched.cases_forest, forest_yC2000 == 1)$carbon_yC
  control_pos_carbon_2021 = dplyr::filter(matched.cases_forest, forest_yC2021 == 1)$carbon_yC
  
  loss_pos_carbon<- dplyr::filter(matched.cases_forest, forest_yC2000 == 1 & forest_yC2021 == 0 )$carbon_yt
  


  
  mean_pos_carbon<- mean(loss_pos_carbon)
  sum_pos_carbon<- sum(loss_pos_carbon)
  
  
  sum_control_pos_carbon_2000<- sum(control_pos_carbon_2000, na.rm=T)
  sum_control_pos_carbon_2021<- sum(control_pos_carbon_2021, na.rm=T)
  
  mean_control_pos_carbon_2000<- mean(control_pos_carbon_2000, na.rm=T)
  mean_control_pos_carbon_2021<- mean(control_pos_carbon_2021, na.rm=T)
  
  Prop_noloss_carbon_control_pos<- (sum_control_pos_carbon_2021/sum_control_pos_carbon_2000)*100
  Prop_carbon_control_pos<- 100-Prop_noloss_carbon_control_pos
  
  
  n_control_pos_carbon <- length(control_pos_carbon_2000)
  prop_losscarb_control_pos <- Prop_carbon_control_pos/100
  z <- qnorm(0.975)
  lower_losscarb_control_pos <- (prop_losscarb_control_pos + z^2 / (2 * n_control_pos_carbon) - z * sqrt((prop_losscarb_control_pos * (1 - prop_losscarb_control_pos)) / n_control_pos_carbon)) / (1 + z^2 / n_control_pos_carbon)
  upper_losscarb_control_pos <- (prop_losscarb_control_pos + z^2 / (2 * n_control_pos_carbon) + z * sqrt((prop_losscarb_control_pos * (1 - prop_losscarb_control_pos)) / n_control_pos_carbon)) / (1 + z^2 / n_control_pos_carbon)
  lower_losscarb_control_pos<- lower_losscarb_control_pos*100
  upper_losscarb_control_pos<- upper_losscarb_control_pos*100
  
  
  
  dispersion_pos_carbon_effect <- list(t1= control_pos_carbon_2000, t2= control_pos_carbon_2021, loss= loss_pos_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "control_pos")
  
  
  # treatment
  treatment_pos_carbon_2000 = dplyr::filter(matched.cases_forest, forest_yT2000 == 1)$carbon_yt
  treatment_pos_carbon_2021 = dplyr::filter(matched.cases_forest, forest_yT2021 == 1)$carbon_yt
  
  sum_treatment_pos_carbon_2000<- sum(treatment_pos_carbon_2000, na.rm=T)
  sum_treatment_pos_carbon_2021<- sum(treatment_pos_carbon_2021, na.rm=T)
  
  Prop_noloss_carbon_treatment_pos<- (sum_treatment_pos_carbon_2021/sum_treatment_pos_carbon_2000)*100
  Prop_carbon_treatment_pos<- 100-Prop_noloss_carbon_treatment_pos
  
  
  n_control_treatment_carbon <- length(treatment_pos_carbon_2000)
  prop_losscarb_control_treatment <- Prop_carbon_treatment_pos/100
  z <- qnorm(0.975)
  lower_losscarb_control_treatment <- (prop_losscarb_control_treatment + z^2 / (2 * n_control_treatment_carbon) - z * sqrt((prop_losscarb_control_treatment * (1 - prop_losscarb_control_treatment)) / n_control_treatment_carbon)) / (1 + z^2 / n_control_treatment_carbon)
  upper_losscarb_control_treatment <- (prop_losscarb_control_treatment + z^2 / (2 * n_control_treatment_carbon) + z * sqrt((prop_losscarb_control_treatment * (1 - prop_losscarb_control_treatment)) / n_control_treatment_carbon)) / (1 + z^2 / n_control_treatment_carbon)
  lower_losscarb_control_treatment<- lower_losscarb_control_treatment*100
  upper_losscarb_control_treatment<- upper_losscarb_control_treatment*100
  
  
  
  
  loss_treatment_pos_carbon<- dplyr::filter(matched.cases_forest, forest_yT2000 == 1 & forest_yT2021 == 0 )$carbon_yt
  
  mean_treatment_pos_carbon<- mean(loss_treatment_pos_carbon)
  sum_treatment_pos_carbon<- sum(loss_treatment_pos_carbon)

  mean_treatment_pos_carbon_2000<- mean(treatment_pos_carbon_2000, na.rm=T)
  mean_treatment_pos_carbon_2021<- mean(treatment_pos_carbon_2021, na.rm=T)
  
  dispersion_carbon_effect <- list(t1= treatment_pos_carbon_2000, t2= treatment_pos_carbon_2021, loss= loss_treatment_pos_carbon) %>% 
    {lapply(names(.), function(x) data.frame(level= x, value= .[[x]]))} %>% rbind.fill() %>% 
    dplyr::mutate(level= factor(level, levels= c("t1", "t2", "loss")), treatment= "treatment")
  
  summary_carbon<- data.frame(fd= c("treatment", "control_pos", "control_pre"),
                              sum_carbon_t1= c(sum_treatment_pos_carbon_2000, sum_control_pos_carbon_2000 , sum_control_pre_carbon_2000),
                              sum_carbon_t2= c(sum_treatment_pos_carbon_2021, sum_control_pos_carbon_2021 , sum_control_pre_carbon_2021),
                              sum_carbon= c(sum_treatment_pos_carbon, sum_pos_carbon, sum_pre_carbon),
                              sum_carbon_prop_noloss= c(Prop_noloss_carbon_treatment_pos, Prop_noloss_carbon_control_pos , Prop_noloss_carbon_control_pre),
                              sum_carbon_prop_loss= c(Prop_carbon_treatment_pos, Prop_carbon_control_pos , Prop_carbon_control_pre),
                              mean_carbon_t1= c(mean_treatment_pos_carbon_2000, mean_control_pos_carbon_2000 , mean_control_pre_carbon_2000),
                              mean_carbon_t2= c(mean_treatment_pos_carbon_2021, mean_control_pos_carbon_2021 , mean_control_pre_carbon_2021),
                              mean_pixel = c(mean_treatment_pos_carbon, mean_pos_carbon, mean_pre_carbon),
                              low_interval= c(lower_losscarb_control_treatment, lower_losscarb_control_pos, lower_losscarb_control_pre), 
                              upper_interval= c(upper_losscarb_control_treatment, upper_losscarb_control_pos, upper_losscarb_control_pre),
                              zval_forest_2000_2021 = c(sign_Model_M_forest_2000_2021_sum, NA, NA),
                              zval_deforest_2000_2021 = c(sign_Model_M_deforest_2000_2021_sum, NA, NA)  ) %>%                         
    dplyr::mutate(type= type_gov, fd= factor(fd, levels=  c("treatment", "control_pos", "control_pre"))) %>% 
    dplyr::mutate(sign_forest_2000_2021= sapply(.$zval_forest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"} else {""} })  ) %>% 
    dplyr::mutate(sign_deforest_2000_2021= sapply(.$zval_deforest_2000_2021, function(x) {
      if(is.na(x)){""}else if(x<0.001){"***"}else if(x<0.05){"**"}else if(x<0.1){"*"}  else {""} } )  )
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  ############################### Figuras VJRP
  dir_work<- this.path::this.path() %>% dirname() # espacio de trabajo donde se almacena el codigo actual
  
  # plot coeffcients forests ~ governanza type
  plot_varsForest<- ggplot(cofs_varsForest, aes(x = Value.Estimate, y = Coefficient)) +
    geom_point(size= 1, color= ifelse( (cofs_varsForest$min_std) > 0, "blue", ifelse( (cofs_varsForest$max_std) < 0, "red", "black")))+
    geom_segment( aes(x= min_std, xend = max_std, 
                      yend = Coefficient), size= 0.5,
                  color= ifelse( (cofs_varsForest$min_std) > 0, "blue", ifelse( (cofs_varsForest$max_std) < 0, "red", "black")))+
    geom_text(
      aes(label = sign_label, x = ifelse(Value.Estimate > 0, max_std, min_std)),
      hjust = ifelse(cofs_varsForest$Value.Estimate > 0, -0.5, 1.5), 
      size = 3, nudge_y = 0.15, 
      color= ifelse( (cofs_varsForest$min_std) > 0, "blue", ifelse( (cofs_varsForest$max_std) < 0, "red", "black")) )+
    labs(x = "Estimates", y = "Variables") +
    scale_x_continuous(limits = c(-1,1))+
    theme(legend.position =  "none",
          plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
          axis.ticks.length   = unit(0.3, "mm"),
          text = element_text(size = 10),
          panel.background = element_rect(fill = NA), panel.grid.major = element_line(color = "gray"),
          axis.line = element_line(size = 0.5, colour = "black") )+
    ggtitle(type_gov) 
  
  
  # dendograma multicolinealidad
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

  # Mejores modelos
  plot_better_model<-  ggplot()+
    geom_tile(data= vars_models, aes(x= model , y= vars, fill = eval(sym(critera)) ), color="black", alpha= 0.5, size=0.2)+
    scale_fill_gradientn(critera, colors = brewer.pal(11, "Spectral"))  
  
  
  # Figura puntajes de propension
  # Prematch propension
  PreMatchingIndexData<- data.frame(abs(std.diff)) %>% set_names("Desequilibrio") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(Desequilibrio)
  PreMatchingIndexDataV2<- PreMatchingIndexData  %>%  arrange(desc(Desequilibrio)) %>%
    mutate(Desequilibrio= ifelse(Desequilibrio>=100,100,Desequilibrio)) %>% 
    mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))
  
  #### Parametros plot valores de desequilibrio prematch
  y_axis_title_unbalanced_vars_prematch<- "Index of covariate imbalance"
  x_axis_title_unbalanced_vars_prematch<- "Variables"
  legend_title_unbalanced_vars_prematch<- "Pixeles de la ventana"
  plot_title_unbalanced_vars_prematch<- "Pre Matching"
  color_vline_unbalanced_vars_prematch<- "red"
  pos_vline_unbalanced_vars_prematch<- 25
  
  ## generar plot  valores de desequilibrio
  PreMatchingIndexPlot<- ggplot(data= PreMatchingIndexDataV2)+  
    geom_segment(aes(x=0,  xend=Desequilibrio, y=Variable, yend=  Variable), size= 0.5) +
    geom_point(aes(x=Desequilibrio, y= Variable), size= 1) +
    labs(x= y_axis_title_unbalanced_vars_prematch, y= x_axis_title_unbalanced_vars_prematch)+
    geom_vline(aes(xintercept= pos_vline_unbalanced_vars_prematch),  size= 0.5, linetype="dashed", color = color_vline_unbalanced_vars_prematch)+
    theme(legend.position =  "none",
          plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
          axis.ticks.length   = unit(0.3, "mm"),
          text = element_text(size = 10),
          panel.background = element_rect(fill = NA), panel.grid.major = element_line(color = "gray"),
          axis.line = element_line(size = 0.5, colour = "black") )+
    scale_x_continuous(expand = c(0,0), limits = c(0,110))+
    ggtitle(plot_title_unbalanced_vars_prematch)
  
  
  # posmatch propension
  # organizar datos puntaje
  posmatchingIndexData<- data.frame(abs(std.diff1)) %>% set_names("Desequilibrio") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(Desequilibrio)
  posmatchingIndexDataV2<- posmatchingIndexData  %>%  arrange(desc(Desequilibrio)) %>%
    mutate(Desequilibrio= ifelse(Desequilibrio>=100,100,Desequilibrio)) %>% 
    mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))
  
  #### Parametros plot valores de desequilibrio
  y_axis_title_unbalanced_vars_posmatch<- "Index of covariate imbalance"
  x_axis_title_unbalanced_vars_posmatch<- "Variables"
  legend_title_unbalanced_vars_posmatch<- "Pixeles de la ventana"
  plot_title_unbalanced_vars_posmatch<- "Pos Matching"
  color_vline_unbalanced_vars_posmatch<- "red"
  pos_vline_unbalanced_vars_posmatch<- 25
  
  ## generar plot  valores de desequilibrio
  posmatchingIndexPlot<- ggplot(data= posmatchingIndexDataV2)+  
    geom_segment(aes(x=0,  xend=Desequilibrio, y=Variable, yend=  Variable), size= 0.5) +
    geom_point(aes(x=Desequilibrio, y= Variable), size= 1) +
    labs(x= y_axis_title_unbalanced_vars_posmatch, y= x_axis_title_unbalanced_vars_posmatch)+
    geom_vline(aes(xintercept= pos_vline_unbalanced_vars_posmatch),  size= 0.5, linetype="dashed", color = color_vline_unbalanced_vars_posmatch)+
    theme(legend.position =  "none",
          plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
          axis.ticks.length   = unit(0.3, "mm"),
          text = element_text(size = 10),
          panel.background = element_rect(fill = NA), panel.grid.major = element_line(color = "gray"),
          axis.line = element_line(size = 0.5, colour = "black") )+
    scale_x_continuous(expand = c(0,0), limits = c(0,110))+
    ggtitle(plot_title_unbalanced_vars_posmatch)
  
  summ_matching_index_plot<- ggpubr::ggarrange(plotlist = list(PreMatchingIndexPlot, posmatchingIndexPlot) )
  
  
  ## V2 - Figura Index of covariate imbalance  
  summ_Imbalancedata<- plyr::rbind.fill(list( dplyr::mutate(PreMatchingIndexDataV2, Match= "Unmatched"),  dplyr::mutate(posmatchingIndexDataV2, Match= "Matched")))
  
  gg_summ_Imbalancedata<- ggplot(data= summ_Imbalancedata)+  
    geom_point(aes(x=Desequilibrio, y= Variable, color= Match), size= 1) +
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
  
  
  
  
  
  
  # Figura desbalance prematch
  includedListPreMatching<- split(data, data[, type_gov])
  
  #### Parametros graficos ps values pre match
  in_area_prematch<- list(title=  paste0("In ", type_gov), color= "goldenrod")
  out_area_prematch<- list(title=  paste0("Out ", type_gov), color= "olivedrab")
  y_axis_title_prematch <- "Number of Units"
  x_axis_title_prematch <- "Propensity Score"
  legend_title_prematch <- "Window Pixels"
  plot_title_prematch <- "Pre Matching"
  
  PreMatchversusplot<- ggplot(data= includedListPreMatching[["0"]])+
    geom_histogram(data= includedListPreMatching[["1"]],  aes(x = psvalue, y = ..count.., 
                                                              fill= in_area_prematch$title, color= in_area_prematch$title )) +
    geom_histogram( aes(x = psvalue, y = -..count.., fill= out_area_prematch$title, color= out_area_prematch$title))+
    scale_y_continuous(labels = abs) + scale_fill_manual(legend_title_prematch, values = alpha(c(out_area_prematch$color, in_area_prematch$color),0.5) ) +
    scale_color_manual(legend_title_prematch, values = alpha(c(out_area_prematch$color, in_area_prematch$color),1) ) +
    labs(x= x_axis_title_prematch, y= y_axis_title_prematch) + ggtitle(plot_title_prematch) +     
    guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5))+
    theme(panel.background = element_rect(fill = NA), 
          panel.grid.major = element_line(color = "gray"),
          legend.position =  "bottom", text = element_text(size = 10),
          panel.border = element_rect(color = NA,fill = NA),
          axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"))
  
  
  # Figura desbalance posmatch
  includedListposmatching<- split(y, y[, type_gov])
  
  #### Parametros graficos ps values pos match
  in_area_posmatch<- list(title= paste0("In ", type_gov), color= "goldenrod")
    out_area_posmatch<- list(title= paste0("Out ", type_gov), color= "olivedrab")
  y_axis_title_posmatch <- "Number of Units"
  x_axis_title_posmatch <- "Propensity Score"
  legend_title_posmatch <- "Window Pixels"
  plot_title_posmatch <- "Pos Matching"
  
  
  posmatchversusplot<- ggplot(data= includedListposmatching[["0"]])+
    geom_histogram(data= includedListposmatching[["1"]],  aes(x = psvalue, y = ..count.., 
                                                              fill= in_area_posmatch$title, color= in_area_posmatch$title )) +
    geom_histogram( aes(x = psvalue, y = -..count.., fill= out_area_posmatch$title, color= out_area_posmatch$title))+
    scale_y_continuous(labels = abs) + scale_fill_manual(legend_title_posmatch, values = alpha(c(out_area_posmatch$color, in_area_posmatch$color),0.5) ) +
    scale_color_manual(legend_title_posmatch, values = alpha(c(out_area_posmatch$color, in_area_posmatch$color),1) ) +
    labs(x= x_axis_title_posmatch, y= y_axis_title_posmatch) +
    scale_x_continuous(limits = c(0,1))+  ggtitle(plot_title_posmatch) +     
    guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5))+
    theme(panel.background = element_rect(fill = NA), 
          panel.grid.major = element_line(color = "gray"),
          legend.position =  "bottom", text = element_text(size = 10),
          panel.border = element_rect(color = NA,fill = NA),
          axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"))
  
  summ_matching_propension_plot<- ggpubr::ggarrange(plotlist = list(PreMatchversusplot, posmatchversusplot), common.legend = T,
                                                    legend = "bottom")
  
  
  
  
  #### Parametros graficos Figura peresistencia de forest
  summary_forest

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
      
  
  plot_forest<- ggplot(data= dataplot_forest,  aes(x= label_x, y= forest_prop_loss , fill= label_fill))+
    geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
    geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                  width = 0.1, position =  position_dodge(width  = .8), color = "black")+
    # geom_signif(position="identity",  textsize = 5, comparisons=list(c("Control posMatching","Treatment")) , annotations = dataplot_forest$sign_forest_2000_2021[1])+
        xlab(x_axis_title_result_forest)+ylab(y_axis_title_result_forest)+
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
    scale_fill_manual(legend_title_result_forest,  values = setNames(guide_fill_forest$color_fill ,guide_fill_forest$label_fill) )+
    theme_minimal()+
    theme(legend.position = "bottom",
          axis.text.x  = element_blank(),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))
  
  
  
  
  
  
  
  
  plot_forest_sign <- ggplot(data= dataplot_forest,  aes(x= label_x, y= forest_prop_loss , fill= label_fill))+
    geom_bar(stat = "identity", width = 0.4, size= 0.1, position = position_dodge(width  = .8)) +
    geom_errorbar(aes(ymin = low_interval, ymax = upper_interval),
                  width = 0.1, position =  position_dodge(width  = .8), color = "black")+
    geom_signif(position="identity",  textsize = 5, comparisons=list(c("Control posMatching","Treatment")) , annotations = dataplot_forest$sign_forest_2000_2021[1])+
    xlab(x_axis_title_result_forest)+ylab(y_axis_title_result_forest)+
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")) +
    scale_fill_manual(legend_title_result_forest,  values = setNames(guide_fill_forest$color_fill ,guide_fill_forest$label_fill) )+
    theme_minimal()+
    theme(legend.position = "bottom",
          axis.text.x  = element_blank(),
          axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))
  
  
  
  
  
  
  
  
  #### Parametros graficos Figura peresistencia de carbon

  guide_fill_carbon<- list(
    data.frame(fd= "control_pre", label_fill= "Control preMatching", color_fill= "lightskyblue1"),
    data.frame(fd= "control_pos", label_fill = "Control posMatching", color_fill= "rosybrown1"),
    data.frame(fd= "treatment", label_fill = "Treatment", color_fill = "lightgoldenrodyellow")
  ) %>% rbind.fill()
  
  guide_xaxis_carbon <- list(
    data.frame(level= "mean_carbon_t1", label_x= "Forests 2000"),
    data.frame(level= "mean_carbon_t2", label_x= "Forests 2021")
  ) %>% rbind.fill()


  data_plot_carbon<-   melt(data.table(summary_carbon)[,c("fd", "mean_carbon_t1", "mean_carbon_t2")], 
                            id.vars = "fd", variable.name = "level", 
                            value.name = "value") %>% 
    list(guide_xaxis_carbon, guide_fill_carbon) %>% join_all() %>% 
    dplyr::mutate(
      label_fill= factor(label_fill, unique(guide_fill_carbon$label_fill)),
      label_x= factor(label_x, levels= unique(guide_xaxis_carbon$label_x))
    )
  
  data_sum_plot_carbon<- melt(data.table(summary_carbon)[,c("fd", "sum_carbon_t1", "sum_carbon_t2")], 
                              id.vars = "fd", variable.name = "level", 
                              value.name = "sum_carbon") %>% dplyr::mutate(level= gsub("sum_", "mean_",level)) %>% 
    list(data_plot_carbon) %>% join_all()
  
  
  y_axis_title_result_carbon_mean<- "Mean Carbon loss by pixel"
  
  plot_carbon<- ggplot()+geom_bar(data= data_plot_carbon,  aes(x= label_x , y= value*100 , fill= label_fill), stat = "identity",
                      width = 0.4, size= 0.1, position = position_dodge(width  = .8) )+
    xlab("")+ylab(y_axis_title_result_carbon_mean)+
    scale_fill_manual("",
                      values = setNames(guide_fill_carbon$color_fill ,
                                        guide_fill_carbon$label_fill) )  +
    theme_minimal()+
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(angle = 90, hjust = 1),
      axis.line.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"))
  
  
  
  
  
  
  
  ### Loss carbon
  
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
  
  
  
  
  #### Parametros graficos figura dispersion pixeles de carbon

  guide_fill_carbon_disperssion<- list(
    data.frame(treatment= "control_pre", label_fill= "Control preMatching", color_fill= "lightskyblue1"),
    data.frame(treatment= "control_pos", label_fill = "Control posMatching", color_fill= "rosybrown1"),
    data.frame(treatment= "treatment", label_fill = "Treatment", color_fill = "lightgoldenrodyellow")
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
  
  limits_axis_y<- boxplot.stats(dataplot_carbon_disperssion$value)$stats %>% {c(min(.), max(.))}

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
  
  

  plot_carbon_final<- ggarrange(plotlist =
                                        list(plot_carbon, plot_loss_carbon,
                                             plot_carbon_disperssion_t1_t2, 
                                             plot_carbon_disperssion_loss),
                                      common.legend = T, legend = "bottom", nrow = 2, ncol=2 ) %>% 
    annotate_figure( bottom = text_grob( paste0("\u25B2: Sum total Carbon loss by pixel" ),
                                         size = 8) )
  

  ## resumen variables analizadas
  plot_response<- ggpubr::ggarrange(plotlist = list(plot_forest, plot_loss_carbon_fin), common.legend = T, legend = "bottom")
  plot_response_sign<- ggpubr::ggarrange(plotlist = list(plot_forest_sign, plot_loss_carbon_sign), common.legend = T, legend = "bottom")
  
  
  
  
  
  propensityScore_data<- list( data.frame(PreMatchingIndexDataV2, type_match= "Pre Matching", type_gov= type_gov)  ,
                               data.frame(posmatchingIndexDataV2, type_match= "Pos Matching", type_gov= type_gov)
  ) %>% plyr::rbind.fill() %>% dplyr::mutate(period= "2000")
  
  openxlsx::write.xlsx(propensityScore_data, paste0(type_gov, "_", "propensityScore_data", ".xlsx") )
  
  
  # Guardar resultados
  setwd(dir_work)
  save.image(paste0(type_gov, ".RData"))
  
  