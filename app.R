source("sample_size_procs.R", local = TRUE, echo = FALSE)

ui <- navbarPage(
  title = em("Sample Size App"),
  
  tabsetPanel(id = "tabs", type = "tabs",
              
              # Choice of sample size procedure tab
              tabPanel(
                title = strong("Choice of Sample Size Procedure"),
                fluidRow(
                  column(4, offset = 2,
                         h1("Choose Procedure"),
                         radioButtons("SProc", label = NULL,
                                      choices = c("Procedure by Dobbin et al.", 
                                                  "Procedure by Doros and Lew"),
                                      selected = "Procedure by Dobbin et al.")
                  )
                ),
                fluidRow(
                  column(4, offset = 2,
                         h2("Choice of Confidence Interval Method"),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Dobbin et al.'",
                             radioButtons("SDb", label = NULL,
                                          choices = c("Generalized Confidence Interval", 
                                                      "Modified Large Sample Confidence Interval"),
                                          selected = "Modified Large Sample Confidence Interval")
                           ),
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Doros and Lew'",
                             radioButtons("SDl", label = NULL,
                                          choices = c("Variance Partitioning Confidence Interval - F method", 
                                                      "Generalized Confidence Interval", 
                                                      "Modified Large Sample Confidence Interval"),
                                          selected = "Variance Partitioning Confidence Interval - F method")
                           ),
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Saito et al.'",
                             radioButtons("SSt", label = NULL,
                                          choices = c("Variance Partitioning Confidence Interval - F method", 
                                                      "Generalized Confidence Interval", 
                                                      "Modified Large Sample Confidence Interval"),
                                          selected = "Modified Large Sample Confidence Interval")
                           )
                         )
                  )
                ),
                fluidRow(
                  column(5, offset = 2,
                         h3("Description"),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Dobbin et al.'",
                             p("This procedure ", HTML("<a href='http://dx.doi.org/10.1016/j.csda.2014.11.010'>[2]</a>"),
                               " aims to find the minimum number of participants, ", em("n"), ", given the number of raters, ", 
                               em("k"), ", to achieve a specified width of the confidence interval around a planned value for the ICC for agreement. Requires specification of a maximum n."),
                             conditionalPanel(
                               condition = "input.SDb == 'Generalized Confidence Interval'",
                               p(em("Note that this option might take some time."))
                             )
                           ),
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Doros and Lew'",
                             p("This procedure ", HTML("<a href='https://doi.org/10.3844/amjbsp.2010.1.8'>[3]</a>"),
                               " aims to find the minimum number of participants, ", em("n"), ", and raters, ", em("k"), 
                               " within a grid of possible (", em("n"), em("k"), ") to achieve a specified width of the confidence interval. Requires specification of k and n."),
                             p(em("Note that this option might take some time."))
                           ),
                           conditionalPanel(
                             condition = "input.SProc == 'Procedure by Saito et al.'",
                             p("This procedure ", HTML("<a href='https://doi.org/10.1002/sim.2294'>[1]</a>"),
                               " aims to find the combination of participants, ", em("n"), ", and raters, ", em("k"), 
                               ", to achieve a specified width of the confidence interval around a planned value for the ICC for agreement. Requires specification of a maximum N = n x k.")
                           )
                         )
                  )
                )
              ),
              
              # Parameter Specification tab
              tabPanel(
                title = strong("Parameter Specification"),
                sidebarLayout(
                  sidebarPanel(
                    width = 4,
                    h3("General Parameters"),
                    sliderInput("rho", label = "Planning value for ICC for agreement:",
                                min = 0.05, max = 0.95, step = 0.01, value = 0.7),
                    numericInput("R", label = "Rater to error variance ratio",
                                 min = 0.001, max = 1000, step = 0.001, value = 0.1),
                    sliderInput("target", label = "Target for the width of the confidence interval",
                                min = 0.1, max = 0.9, step = 0.01, value = 0.4),
                    sliderInput("alpha", label = "Confidence level",
                                min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                    numericInput("Sims", label = "Number of simulations",
                                 min = 10, max = 1e4, step = 10, value = 2500),
                    actionButton("computeSS", "Calculate Sample Size")
                  ),
                  mainPanel(
                    conditionalPanel(
                      condition = "input.SProc == 'Procedure by Dobbin et al.'",
                      conditionalPanel(
                        condition = "input.SDb == 'Generalized Confidence Interval'",
                        numericInput("SimsW", label = "Number of simulations within:",
                                     min = 10, max = 1000, step = 1, value = 100)
                      ),
                      numericInput("k", label = "Number of raters/repetitions per participant:",
                                   min = 2, max = 100, step = 1, value = 5),
                      numericInput("n_min", label = "Minimum number of participants for search:",
                                   min = 5, max = 5000, step = 1, value = 6),
                      numericInput("n_max", label = "Maximum number of participants for search:",
                                   min = 5, max = 5000, step = 1, value = 200),
                      numericInput("seed1", label = "Seed:",
                                   min = 0, max = Inf, step = 1, value = 1237)
                    ),
                    conditionalPanel(
                      condition = "input.SProc == 'Procedure by Doros and Lew'",
                      numericInput("k2", label = "Number of raters/repetitions per participant:",
                                   min = 2, max = 100, step = 1, value = 5),
                      numericInput("n_min2", label = "Minimum number of participants for search:",
                                   min = 5, max = 5000, step = 1, value = 5),
                      numericInput("n_max2", label = "Maximum number of participants for search:",
                                   min = 5, max = 5000, step = 1, value = 100),
                      numericInput("seed2", label = "Seed:",
                                   min = 0, max = Inf, step = 1, value = 123)
                    ),
                    conditionalPanel(
                      condition = "input.SProc == 'Procedure by Saito et al.'",
                      numericInput("Nmin", label = "Minimum number of total observations for search:",
                                   min = 10, max = 5000, step = 1, value = 10),
                      numericInput("Nmax", label = "Maximum number of total observations for search:",
                                   min = 10, max = 5000, step = 1, value = 200),
                      numericInput("seed3", label = "Seed:",
                                   min = 0, max = Inf, step = 1, value = 123)
                    ),
                    tableOutput("Smps"),
                    div(textOutput("Tmps"), style = "color:red"),
                    div(textOutput("Tmps"), style = "color:red"),
                    div(textOutput("Tmps"), style = "color:red")
                  )
                )
              ),
              
              # Asymptotics tab
              tabPanel(
                title = strong("Asymptotics"),
                fluidRow(
                  column(4, offset = 2, h3("Confidence Interval Method"),
                         radioButtons("Asy", label = NULL,
                                      choices = c("Generalized Confidence Interval", 
                                                  "Modified Large Sample Confidence Interval", 
                                                  "Variance Partitioning Confidence Interval - F method"),
                                      selected = "Variance Partitioning Confidence Interval - F method")
                  )
                ),
                fluidRow(
                  column(4, offset = 2, h3("Parameters"),
                         sliderInput("asyalpha", label = "Confidence level",
                                     min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                         numericInput("asyk", label = "Number of raters/repetitions per participant:",
                                      min = 2, max = 100, step = 1, value = 5),
                         sliderInput("asyrho", label = "Planning value for ICC for agreement:",
                                     min = 0.05, max = 0.95, step = 0.01, value = 0.7),
                         numericInput("asyR", label = "Rater to error variance ratio",
                                      min = 0.001, max = 1000, step = 0.001, value = 0.1),
                         numericInput("asynsims", label = "Number of simulations",
                                      min = 1, max = 1e6, step = 1, value = 1000),
                         actionButton("computeAs", "Calculate Asymptotic Width")
                  )
                ),
                fluidRow(
                  column(6, offset = 2,
                         tableOutput("asyCIs"),
                         div(textOutput("Tmps"), style = "color:red")
                  )
                )
              )
  )
)


server <- function(input, output, session) {
  observeEvent(input$computeSS, {
    dataSS <- function(){
      alpha <- 1 - as.numeric(input$alpha)
      rho   <- as.numeric(input$rho)
      R     <- as.numeric(input$R)
      Sims  <- as.numeric(input$Sims)
      target <- as.numeric(input$target)
      cat(input$SProc,"\n")
      
      method <- ""
      rt <- NULL
      if(input$SProc == "Procedure by Dobbin et al."){
        if(input$SDb == "Generalized Confidence Interval"){
          method <- "GCI"
        }else if(input$SDb == "Modified Large Sample Confidence Interval"){
          method <- "MLSA"
        }
      }else if(input$SProc =="Procedure by Doros and Lew"){
        if(input$SDl =="Variance Partitioning Confidence Interval - F method"){
          method <- "variance.partition"
        }else if(input$SDl =="Modified Large Sample Confidence Interval"){
          method <- "MLSG"
        }else if(input$SDl =="Generalized Confidence Interval"){
          method <- "GCI"
        }
      }
      cat(method,"\n")
      # Compute based on selected procedure
      if(input$SProc == 'Procedure by Dobbin et al.'){
        withProgress(message = 'Computing', style = 'notification', value = 0, {
          ss <- samplesize.dobbin(rho = rho, R = R, k = as.numeric(input$k), target = target,
                                  alpha = alpha, max_n = as.numeric(input$n_max),
                                  min_n = as.numeric(input$n_min), reps = Sims,
                                  reps_VC = as.numeric(input$SimsW), method = method,
                                  seed = as.numeric(input$seed1))
          rt <- list(n = ss$final, k = as.numeric(input$k), wd = ss$final.val, 
                     target = target, rho = rho, R = R, method = method)
        })
      } else if(input$SProc == 'Procedure by Doros and Lew'){
        withProgress(message = 'Computing', style = 'notification', value = 0, {
          ss <- samplesize.doros(rho = rho, R = R, k = as.numeric(input$k2), target = target,
                                 alpha = alpha, n_max = as.numeric(input$n_max2),
                                 n_min = as.numeric(input$n_min2), nsims = Sims,
                                 method = method, seed.start = as.numeric(input$seed2), verbose = TRUE)
          rt <- list(n = ss$Sample.Size, k = as.numeric(input$k2), wd = ss$Final.Val, 
                     target = target, rho = rho, R = R, method = method)
        })
      } else if(input$SProc == 'Procedure by Saito et al.'){
        withProgress(message = 'Computing', style = 'notification', value = 0, {
          ss <- samplesize.saito(rho = rho, R = R, target = target, alpha = alpha,
                                 nsims = Sims, N_max = as.numeric(input$Nmax),
                                 N_min = as.numeric(input$Nmin), method = method,
                                 seed.start = as.numeric(input$seed3), verbose = FALSE)
          rt <- list(n = ss$n, k = ss$k, wd = ss$Width, target = target, 
                     rho = rho, R = R, method = method)
        })
      }
      return(rt)
    }
    
    SS <- dataSS()
    
    output$Smps <- renderTable({ 
      data.frame("&#961;" = SS$rho, "R" = SS$R, "&#969;" = SS$target,
                 "n" = as.integer(SS$n), "k" = as.integer(SS$k),
                 "width" = format(SS$wd, digits = 3), check.names = FALSE)
    }, sanitize.text.function = function(x) x, rownames = FALSE)
    
    output$Tmps <- renderText({
      if (!is.null(SS$wd) & SS$wd > as.numeric(input$target)) {
        "Warning! Sample size within supplied search range for the number of participants is not possible."
      }
    })
  })
  
  dataAsy <- observeEvent(input$computeAs, {
    asyk <- as.numeric(input$asyk)
    asyR <- as.numeric(input$asyR)
    asyrho <- as.numeric(input$asyrho)
    asynsims <- as.numeric(input$asynsims)
    method <- ""
    rt <- NULL
    
    if (!(is.null(input$Asy)) & input$Asy == 'Generalized Confidence Interval') {
      method <- "GCI" 
    } else if (!(is.null(input$Asy)) & input$Asy == 'Modified Large Sample Confidence Interval') {
      method <- "MLSG" 
    } else if (!(is.null(input$Asy)) & input$Asy == 'Variance Partitioning Confidence Interval - F method') {
      method <- "VPF" 
    }
    withProgress(message = 'Computing', style = 'notification', value = 0, {
      widths <- nINF(n = 1e3, k = asyk, rho = asyrho, R = asyR, method = method,
                     alpha = 1 - as.numeric(input$asyalpha), nsims = asynsims)
      rt <- list(k = asyk, Mean = widths[[1]], SD = widths[[2]])
    })
    return(rt)
  })
  
  
  dataRead <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    return(CIdata(input$upload$datapath, 
                  alpha = 1-as.numeric(input$dataalpha)))
  })
  
  observeEvent(input$calc,{
    output$Asmps<-renderTable({ 
      data.frame("&#961;"=as.numeric(input$asyrho),
                 "R"=as.numeric(input$asyR),
                 "k"=as.integer(dataAsy()$k),
                 "Mean"= format(dataAsy()$Mean, digits=3),
                 "SD"= format(dataAsy()$SD, digits=3),
                 check.names = FALSE)
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
  })
  
  
  
  output$nfo <- renderTable({
    data.frame("Participants" = dataRead()$n,
               "Raters"       = dataRead()$k,
               "&#961; (A,1)" = dataRead()$ICCA,
               "&#961; (C,1)" = dataRead()$ICCC,
               "R"            = dataRead()$R,
               check.names=FALSE)},
    sanitize.text.function = function(x) x,
    rownames = FALSE)
  
  output$Cmps <- renderTable({
    data.frame("VPF" = dataRead()$ciVPF,
               "MLSG" = dataRead()$ciMLSG,
               "GCI"  = dataRead()$ciGCI,
               check.names=FALSE)
  },
  sanitize.text.function = function(x) x,
  rownames = FALSE)
  
}

shinyApp(ui, server)

