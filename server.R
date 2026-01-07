#ui.R
library(MASS)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(DT)
library(brglm2)
library(readxl)
library(rlang)

brglmControl(maxit = 5000)

simData <-function(exp =1:3, rep = 1:5, dilu = seq(0,-7,by = -1)){
  expand.grid(exp= exp, rep = rep, dilution = dilu) %>% add_column(Ct = rnorm(n = nrow(.),38,5))
}



shapeWide <- function(df){
  df %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>% 
    filter(if_any(starts_with("Ct"), ~ !is.na(.))) %>%   # keep rows with ≥1 Ct
    pivot_longer(
      cols = starts_with("Ct"),
      names_to = "replicate",
      values_to = "Ct"
    ) %>%
    mutate(
      replicate = as.integer(sub("Ct", "", replicate))
    )
}



add_glmmTMB_preds <- function(mod,
                              level        = 0.95,
                              newdata = NULL,
                              type = "response",
                              prefix = "pred_",
                              include_PI   = TRUE,
                              re.form = NA,
                              allow.new.levels = FALSE, 
                              decimal = 2) {
  if (is.null(newdata)) {
    newdata <- stats::model.frame(mod)
  }
  
  # z value
  z <- qnorm(1 - (1 - level) / 2)
  
  # population-level predictions (fixed effects only)
  pop <- predict(
    mod,
    newdata = newdata,
    type    = "response",
    re.form = re.form,
    se.fit  = TRUE
  )
  
  # residual variance (single pooled variance)
  sigma  <- sigma(mod)
  sigma2 <- sigma^2
  
  # add mean prediction + CI
  newdata[[paste0(prefix, "mean")]] <- round(pop$fit, decimal)
  newdata[[paste0(prefix, "lwr")]]  <-  round(pop$fit - z * pop$se.fit, decimal)
  newdata[[paste0(prefix, "upr")]]  <-  round(pop$fit + z * pop$se.fit, decimal)
  
  # optional prediction interval
  if (include_PI) {
    newdata[[paste0(prefix, "pi_lwr")]] <-
      round(pop$fit - z * sqrt(pop$se.fit^2 + sigma2), decimal)
    
    newdata[[paste0(prefix, "pi_upr")]] <-
      round( pop$fit + z * sqrt(pop$se.fit^2 + sigma2), decimal)
  }
  
  newdata

}


plot_fits_by_exp_and_overall <- function(mod,
                                        data       = NULL,
                                        level      = 0.95,
                                        show_ci    = TRUE,
                                        show_points = TRUE,
                                        decimal = 2) {
  if (is.null(data)) data <- stats::model.frame(mod)

  data <- data %>%
    mutate(exp = factor(exp)) %>%
    arrange(exp, dilution)

  # experiment-specific (conditional) fits
  d_cond <- add_glmmTMB_preds(
    mod,
    level = level,
    newdata = data,
    prefix = "cond_",
    include_PI = FALSE,
    re.form = NULL,
    decimal = decimal
  )

  # overall (population) fits + CI
  d_pop <- add_glmmTMB_preds(
    mod,
    level = level,
    newdata = data,
    prefix = "pop_",
    include_PI = FALSE,
    re.form = NA,
    decimal
  )

  # one row per dilution for the overall line/ribbon
  pop_df <- d_pop %>%
    distinct(dilution, .keep_all = TRUE) %>%
    arrange(dilution)

  p <- ggplot(d_cond, aes(x = dilution, y = Ct)) +
    { if (show_points) geom_point(aes(colour = exp), alpha = 0.6) } +
    geom_line(aes(y = cond_mean, colour = exp, group = exp), linewidth = 0.9) +
    geom_line(
      data = pop_df,
      aes(x = dilution, y = pop_mean),
      inherit.aes = FALSE,
      linewidth = 1.2,
      colour = "black"
    ) +
    labs(x = "Dilution", y = "Ct", colour = "Experiment") +
    theme_minimal()

  if (show_ci) {
    p <- p +
      geom_ribbon(
        data = pop_df,
        aes(x = dilution, ymin = pop_lwr, ymax = pop_upr),
        inherit.aes = FALSE,
        alpha = 0.2
      )
  }

  list(p = p, d_cond = d_cond, d_pop = d_pop)
}


shinyServer(function(input, output) {


getNames <- reactive({
    inFile1 <- input$file1  
    if (is.null(inFile1)) return(NULL) 
    theD <- readxl::read_xlsx(inFile1$datapath)
    theNames = names(theD)
    info = list(theNames = theNames, N = nrow(theD))
 
  return(info)
    
  })
  
  
  
Data <- reactive({
  ###read user input file  
  inFile1 <- input$file1  
  if (is.null(inFile1)) return(NULL) 
  theD <- readxl::read_xlsx(inFile1$datapath)
  info = list( dd = theD)
  
  theD = shapeWide(theD)
  myD <- theD %>%
    select(Ct, dilution = all_of(input$D), exp = all_of(input$Y)) %>%
    mutate(
      exp  = as.factor(exp),
      dilu = as.factor(dilution)
    )
  
  # keep only selected experiments (if user has picked)
  if (!is.null(input$exp_keep) && length(input$exp_keep) > 0) {
    myD <- myD %>% filter(exp %in% input$exp_keep)
  }
  

  
  fit_aov <- aov(Ct ~ dilution + exp, data = myD)
  sd <- sqrt(summary(fit_aov)[[1]]["Residuals", "Mean Sq"])
  
  mod <-  glmmTMB::glmmTMB(
    Ct ~ dilution + (1|exp),
    dispformula = ~ 0 + dilu,
    data    = myD,
    family = gaussian()
  )

  disp <- summary(mod)$coefficients$disp[,1]

  # Convert to actual variances
  var_by_dilution <- exp(disp)

  # Make a tidy data.frame
  var_df <- data.frame(
    dilution = names(var_by_dilution),
    SD = sqrt(as.numeric(var_by_dilution)),
    row.names = NULL
  )
  
  
  
  mod <-  glmmTMB::glmmTMB(
    Ct ~ dilution + (1|exp),
    #dispformula = ~ 0 + dilu,
    data    = myD,
    family = gaussian()
  )
  
  var_df <- var_df %>% rbind(data.frame(dilution = "All",SD =  sigma(mod))) %>% rbind(data.frame(dilution = "ANOVA",SD = sd)) %>%
  mutate(SD = round(SD, input$decimal))# residual SD

  
  r <- plot_fits_by_exp_and_overall(mod, myD, level = 0.95, show_ci = TRUE, decimal = input$decimal)
  
 
  
  return(info = list(dd = r$d_pop, var = var_df, plot = r$p))
  })
  
 


 output$namesN <- renderUI({
   theNames = getNames()$theNames   
   theL = as.list(theNames);names(theL) = theNames
   selectInput("N", label = h5("Select column for replicates"), 
               choices = theL, selected = theL[1])
   
 })


 
 
 output$namesY <- renderUI({
   theNames = getNames()$theNames   
   theL = as.list(theNames);names(theL) = theNames
   selectInput("Y", label = h5("Select column for experiments"), 
               choices = theL, selected = theL[1])
   
 })
 
 

 
 
 
 output$namesD <- renderUI({
   theNames = getNames()$theNames   
   theL = as.list(theNames);names(theL) = theNames
   selectInput("D", label = h5("Select column for Dilution"), 
               choices = theL, selected = theL[2])
   
 })


 output$expLevels <- renderUI({
   req(input$file1, input$Y)
   
   theD <- readxl::read_xlsx(input$file1$datapath)
   
   # experiment column selected by user
   exp_vec <- theD[[input$Y]]
   
   # unique choices (keep as character)
   choices <- sort(unique(as.character(exp_vec)))
   choices <- choices[!is.na(choices) & choices != ""]
   
   shinyWidgets::pickerInput(
     inputId  = "exp_keep",
     label    = "Select experiment(s) to include",
     choices  = choices,
     selected = choices,      # default: include all
     multiple = TRUE,
     options  = list(`actions-box` = TRUE, `live-search` = TRUE)
   )
 })
 
 output$namesCT <- renderUI({
   theNames = getNames()$theNames   
   theL = as.list(theNames);names(theL) = theNames
   selectInput("CT", label = h5("Select column for Ct values"), 
               choices = theL, selected = theL[4])
   
 })
 
 
 
output$plot <- renderPlot({
  table = round(Data()$tt,3)
  
  if(!sum(dim(table)== c(1,1))==2) {
 
  y = table$Estimate[table$label==input$p]
  rownames(table) = paste(table$Estimate, "(", table$LowerCI, ",", table$UpperCI, ")",sep="")
  
  fp <- ggplot(data=table, aes(x=label, y=Estimate, ymin=LowerCI, ymax=UpperCI,label = rownames(table))) +
    geom_pointrange(fill='cyan4', color='grey', shape=21, fatten = 1, size = 2,alpha = 0.5, lwd = 1) + 
    geom_point(color='cyan4',  shape=21,size = 2,alpha = 0.8) + 
    geom_hline(aes(yintercept=y),color="red", linetype="dashed", size=1)  +
    geom_vline(aes(xintercept=as.numeric(input$p)),color="red", linetype="dashed", size=1)  +
    coord_flip() +  
    geom_text(data = table[table$label==input$p,],size =6, hjust=1.5,vjust = 1.5,color="red",
              aes(label,Estimate,label=rownames(table[table$label==input$p,]))) +
    xlab("Percentile") + ylab("Estimate (95% CI)") + labs(colour = "Experiment") +
    theme(axis.text=element_text(size=8),
           axis.title=element_text(size=14,face="bold"))+
    scale_x_continuous(breaks = unique(table$label))
  
  
  fp
  }
  
})








output$plotly <- renderPlotly({
    
    ggplotly(Data()$p)
  
  
})








output$table <- DT::renderDataTable(DT::datatable({
  temp1 = Data()$dd

  if(!is.null(temp1)) temp1 = data.frame(temp1) else temp1 = data.frame()
  if (nrow(temp1)==0) { data.frame(No_Data=NULL) }else{
    temp1
  }
  },rownames = F,editable = F,options = list(lengthChange = T, pageLength = 100)))





output$table1 <- DT::renderDataTable(DT::datatable({
  temp1 = Data()$var
  if(!is.null(temp1)) temp1 = data.frame(temp1) else temp1 = data.frame()
  if (nrow(temp1)==0) { data.frame(No_Data=NULL) }else{
    temp1
  }
},rownames = F,editable = F,options = list(lengthChange = T, pageLength = 15)))


# 
# output$tableRR <- DT::renderDataTable({
#   temp = Data()
#   
#   temp1 =temp$tt
#   temp1$label = NULL
#   index= temp$index-1
# 
#   
#   if(!is.null(temp1)) temp1 = data.frame(temp1) else temp1 = data.frame()
#   if (nrow(temp1)==0) { data.frame(No_Data=NULL) }else{
#     temp1
#   }
#   
#   js <- paste0("function(row, data) {
#                 $(this
#                      .api()
#                      .row(", index, ")
#                      .node())
#                 .css({'background-color': 'lightblue'});}")
#   
#   
#   
#   
#   DT::datatable(temp1,rownames = T,editable = F,options = list(lengthChange = T, pageLength = 35,drawCallback=JS(js)))
# 
# 
# })






# 
# 
# output$tableRRC <- DT::renderDataTable({
#   temp = Data()
#   
#   temp1 =temp$cc
#   temp1$label = NULL
#   index= temp$index-1
#   
#   
#   if(!is.null(temp1)) temp1 = data.frame(temp1) else temp1 = data.frame()
#   if (nrow(temp1)==0) { data.frame(No_Data=NULL) }else{
#     temp1
#   }
#   
#   js <- paste0("function(row, data) {
#                 $(this
#                      .api()
#                      .row(", index, ")
#                      .node())
#                 .css({'background-color': 'lightblue'});}")
#   
#   
#   
#   
#   DT::datatable(temp1,rownames = T,editable = F,options = list(lengthChange = T, pageLength = 35,drawCallback=JS(js)))
#   
#   
# })
# 
# 
# 






    
  })
  

