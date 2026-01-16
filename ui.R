# ui.R
library(MASS)
library(plotly)
library(shinyWidgets)
library(shinythemes)

shinyUI(
  fluidPage(
    theme = shinytheme("spacelab"),
    
    titlePanel(
      div(
        img(src = "PHO.png", height = 80, width = 240),
        "Regression with Ct values"
      ),
      windowTitle = "ANALYSIS OF Ct"
    ),
    
    withMathJax(),
    
    fluidRow(
      column(
        3,
        fileInput(
          'file1',
          label = h5('Upload data in xlsx format:'),
          accept = c(".xlsx")
        ),
        
        uiOutput("namesY"),
        uiOutput("namesD"),
        uiOutput("expLevels"),
        
        numericInput(
          "decimal",
          h5("Decimal places to display:"),
          value = 2,
          min = 0,
          max = 10
        )
      ),
      
      column(
        9,
        mainPanel(
          tabsetPanel(
            
            tabPanel(
              "Data", 
              DT::dataTableOutput("table"),
              value = "hide"
            ),
            
            tabPanel(
              "Variance by dilution", 
              DT::dataTableOutput("table1"),
              value = "hide"
            ),
            
            tabPanel(
              "Plotly",
              plotlyOutput("plotly", height = "800px"),
              value = "hide"
            ),
            
            tabPanel(
              "About",
              fluidRow(
                column(
                  12,
                  h4("About this app"),
                  p(
                    "This app analyses real-time PCR data using Ct values with a regression/mixed-model framework. ",
                    "Ct is treated as a continuous outcome and modelled as a function of dilution, ",
                    "with experiments (runs/plates) included as a random effect."
                  ),
                  
                  h4("Statistical model"),
                  p(
                    "The main model is a Gaussian mixed model of the form ",
                    tags$code("Ct ~ dilution + (1 | exp)"),
                    ", fitted with ",
                    tags$code("glmmTMB"),
                    "."
                  ),
                  tags$ul(
                    tags$li(
                      strong("Fixed effect of dilution: "),
                      "captures how the mean Ct changes with dilution."
                    ),
                    tags$li(
                      strong("Random intercept for exp: "),
                      "allows each experiment (run/plate) to have its own baseline Ct, ",
                      "capturing between-experiment variability."
                    )
                  ),
                  p(
                    "In one version of the model, a dispersion formula ",
                    tags$code("dispformula = ~ 0 + dilu"),
                    " is used so that the residual variance can differ by dilution.",
                    " In the simpler model without a dispersion formula, a single residual variance is assumed across all dilutions."
                  ),
                  
                  h4("Interpretation of the SDs shown in the app"),
                  p("The app summarises several standard deviations (SD) that correspond to different sources of variation."),
                  
                  h5("By-dilution SDs from the dispersion model"),
                  tags$ul(
                    tags$li(
                      strong("dilu1, dilu2, …: "),
                      "these SDs come from the model with ",
                      tags$code("dispformula = ~ 0 + dilu"),
                      ". ",
                      "Each value represents the residual SD of Ct for that specific dilution, ",
                      "after adjusting for the fixed effect of dilution and the experiment random effect. ",
                      "They describe how variable Ct is at each dilution level."
                    )
                  ),
                  
                  h5("Mixed-model SDs (from glmmTMB without dispersion model)"),
                  tags$ul(
                    tags$li(
                      strong("intra: "),
                      "the within-experiment (residual) SD from the homogeneous-variance mixed model. ",
                      "It reflects repeatability: how much individual Ct values vary around the fitted mean within the same experiment."
                    ),
                    tags$li(
                      strong("inter: "),
                      "the between-experiment SD (random intercept SD for ",
                      tags$code("exp"),
                      "). ",
                      "This describes how much experiment-level baselines differ from run to run."
                    ),
                    tags$li(
                      strong("total: "),
                      "the combined SD for a new Ct measurement when you do not know which experiment it will come from. ",
                      "It is computed as ",
                      HTML("&#8730;(intra<sup>2</sup> + inter<sup>2</sup>).")
                    )
                  ),
                  
                  h5("ANOVA-based SDs (fixed-effects view)"),
                  p(
                    "For comparison, an ordinary linear model ",
                    tags$code("lm(Ct ~ dilution + exp)"),
                    " is also fitted and an ANOVA table is used to derive variance components, ",
                    "treating ",
                    tags$code("exp"),
                    " as if it were a random factor in a classical ANOVA framework."
                  ),
                  tags$ul(
                    tags$li(
                      strong("ANOVA_within: "),
                      "square root of the residual mean square from the ANOVA. ",
                      "This is the within-experiment SD in the fixed-effects view and is comparable to ",
                      tags$code("intra"),
                      "."
                    ),
                    tags$li(
                      strong("ANOVA_between: "),
                      "an ANOVA-based estimate of the between-experiment SD, ",
                      "derived from the difference between the experiment mean square and the residual mean square ",
                      "using the expected mean squares approach (assuming a roughly balanced design). ",
                      "This is conceptually comparable to ",
                      tags$code("inter"),
                      ", though estimated in a different way."
                    ),
                    tags$li(
                      strong("ANOVA_total: "),
                      "the total SD combining ANOVA_between and ANOVA_within, ",
                      "computed as ",
                      HTML("&#8730;(ANOVA_between<sup>2</sup> + ANOVA_within<sup>2</sup>). "),
                      "This is the ANOVA analogue of ",
                      tags$code("total"),
                      "."
                    )
                  ),
                  
                  h4("How to interpret these in practice"),
                  tags$ul(
                    tags$li(
                      strong("By-dilution SDs (dilu1–diluK): "),
                      "tell you how variable Ct is at each dilution level."
                    ),
                    tags$li(
                      strong("intra / ANOVA_within: "),
                      "repeatability: variation of Ct within the same experiment/run."
                    ),
                    tags$li(
                      strong("inter / ANOVA_between: "),
                      "reproducibility across experiments: how much runs/plates differ in their baseline Ct."
                    ),
                    tags$li(
                      strong("total / ANOVA_total: "),
                      "overall measurement uncertainty for a single new Ct value in routine use, ",
                      "combining both within- and between-experiment variation."
                    )
                  )
                )
              ),
              value = "hide"
            ),
            
            id = "tabs"
          )
        )
      )
    )
  )
)
