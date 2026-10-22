library(shiny)
library(plotly)
library(cusp)
library(zip)
library(gridExtra)


DA_DEMO_PATH <- "data/demo_data.csv"

dataAnalysisUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::hr(),
    shiny::h3("Fit a Cusp Model to Data"),
    shiny::p("Upload independent observations with a continuous response. Choose numeric predictors for the normal and splitting controls. Binary responses are not supported. Have fun!"),
    shiny::fileInput(ns("file"), "CSV dataset", accept = ".csv"),
    shiny::actionButton(ns("use_demo"), "Use the Demo Dataset"),
    shiny::p(class = "note", "This demo dataset is based on the cusp model of attitude change. The variables are: responses - attitude, normal variable - information, splitting variable - attention. For theoretical background, see van der Maas et al. (2003)."),
    shiny::p("Use commas between columns and decimal points for numbers. Missing or non-finite values in selected variables are excluded."),
    shiny::uiOutput(ns("variables")),
    shiny::actionButton(ns("fit"), "Fit cusp model", class = "btn-primary"),
    shiny::verbatimTextOutput(ns("status")),
    shiny::h4("Parameter estimates"),
    shiny::tableOutput(ns("coefficients")),
    shiny::h4("Fit information"),
    shiny::tableOutput(ns("metrics")),
    shiny::checkboxInput(ns("compare"), "Compare with linear regression (AIC and BIC)", FALSE),
    shiny::conditionalPanel(
      condition = "input.compare", ns = ns,
      shiny::tableOutput(ns("comparison")),
      shiny::p("Linear model: response ~ normal predictor + splitting predictor, fitted to the same standardised data. Lower AIC and BIC are better; delta is the difference from the better model. These criteria trade fit against the number of parameters.")
    ),
    shiny::checkboxInput(ns("observed"), "Show transformed observed responses", TRUE),
    shiny::checkboxInput(ns("projected"), "Show projections onto stable equilibria", TRUE),
    plotly::plotlyOutput(ns("surface"), height = "600px"),
    shiny::p("Grey dots are transformed observations and need not lie on the surface. Purple dots place each observation on its nearest stable equilibrium, keeping its estimated controls fixed. This uses the observed response to choose a branch: it is a descriptive projection, not an independent prediction or evidence of hysteresis."),
    shiny::h4("Respondent coordinates (first 5 rows)"),
    shiny::tableOutput(ns("coordinates")),
    shiny::downloadButton(ns("download"), "Download all fitted coordinates"),
    shiny::downloadButton(
      ns("download_all"),
      "Download all analysis results"
    ),
    shiny::p("A successful fit does not prove a catastrophe model is correct. The app assumes that the data-generating mechanism is a cusp, which need not be the case. A good fit shows only that the cusp describes the data well, not that it is the true mechanism. As always, check theory and consider alternative models.")
  )
}

# Helpers are prefixed with da_ to avoid collisions with base_model.R functions.

# Nearest stable equilibrium to the observed state, at given alpha and beta.
da_nearest_stable <- function(alpha, beta, observed) {
  roots <- polyroot(c(-alpha, -beta, 0, 1))
  real <- sort(Re(roots[abs(Im(roots)) < 1e-6]))
  stable <- real[3 * real^2 - beta >= -1e-7]
  if (!length(stable)) return(NA_real_)
  stable[which.min(abs(stable - observed))]
}

# Range of the values, widened a little so points are not on the plot edge.
da_expanded_range <- function(values) {
  r <- range(values, finite = TRUE)
  padding <- max(diff(r) * 0.12, 0.25)
  r + c(-padding, padding)
}

# Check if variables/responses are standardised
da_axis_label <- function(name, standardised) {
  if (isTRUE(standardised)) paste0(name, " (standardised)") else name
}
# Builds the 3D plot: cusp surface, fold curve, and respondent points.
da_plot <- function(points, show_observed, show_projected, var_names, standardised = TRUE) {
  # Variable names are copied onto the graph
  label_a <- da_axis_label(var_names[2], standardised)  # alpha predictor
  label_b <- da_axis_label(var_names[3], standardised)  # beta predictor
  label_x <- da_axis_label(var_names[1], standardised)  # response / state
  ar <- da_expanded_range(points$alpha)
  br <- da_expanded_range(c(points$beta, 0))
  # Roots at the observed controls, used to size the vertical axis.
  roots <- unlist(lapply(seq_len(nrow(points)), function(i) {
    r <- polyroot(c(-points$alpha[i], -points$beta[i], 0, 1))
    Re(r[abs(Im(r)) < 1e-6])
  }))
  zr <- da_expanded_range(c(points$state, points$stable_projection, roots))
  z <- seq(zr[1], zr[2], length.out = 120)
  b <- seq(br[1], br[2], length.out = 80)
  Z <- matrix(rep(z, length(b)), nrow = length(z))
  B <- matrix(rep(b, each = length(z)), nrow = length(z))
  A <- Z^3 - B * Z
  stable <- Z
  unstable <- Z
  outside <- A < ar[1] | A > ar[2]
  stable[3 * Z^2 - B <= 0 | outside] <- NA_real_
  unstable[3 * Z^2 - B >= 0 | outside] <- NA_real_
  
  p <- plotly::plot_ly()
  p <- plotly::add_surface(p, x = A, y = B, z = stable,
                           name = "Stable equilibria", opacity = 0.55, showscale = FALSE,
                           colorscale = list(c(0, "#93c5fd"), c(1, "#93c5fd")),
                           hovertemplate = paste0(label_a, ": %{x:.3f}<br>", label_b, ": %{y:.3f}<br>",
                                                  label_x, ": %{z:.3f}<extra>Stable</extra>"))
  p <- plotly::add_surface(p, x = A, y = B, z = unstable,
                           name = "Unstable equilibria", opacity = 0.25, showscale = FALSE,
                           colorscale = list(c(0, "#fdba74"), c(1, "#fdba74")),
                           hovertemplate = paste0(label_a, ": %{x:.3f}<br>", label_b, ": %{y:.3f}<br>",
                                                  label_x, ": %{z:.3f}<extra>Unstable</extra>"))
  
  # Fold curve: alpha = -2 x^3, beta = 3 x^2
  f <- seq(-sqrt(max(br[2], 0) / 3), sqrt(max(br[2], 0) / 3), length.out = 400)
  fa <- -2 * f^3
  fb <- 3 * f^2
  keep <- fa >= ar[1] & fa <= ar[2] & fb >= br[1] & fb <= br[2]
  f[!keep] <- NA_real_
  p <- plotly::add_trace(p, x = fa, y = fb, z = f,
                         type = "scatter3d", mode = "lines", name = "Folds",
                         line = list(color = "#334155", width = 5), hoverinfo = "skip")
  
  hover <- paste0("CSV row ", points$row,
                  "<br>", var_names[1], ": ", signif(points$response, 4),
                  "<br>", label_a, ": ", signif(points$alpha, 4),
                  "<br>", label_b, ": ", signif(points$beta, 4))
  if (show_observed) {
    p <- plotly::add_trace(p, x = points$alpha, y = points$beta,
                           z = points$state, type = "scatter3d", mode = "markers",
                           name = "Observed (transformed)", marker = list(color = "#475569", size = 4),
                           text = hover, hovertemplate = paste0("%{text}<br>", label_x, ": %{z:.3f}<extra>Observed</extra>"))
  }
  if (show_projected) {
    p <- plotly::add_trace(p, x = points$alpha, y = points$beta,
                           z = points$stable_projection, type = "scatter3d", mode = "markers",
                           name = "Nearest stable projection", marker = list(color = "#7c3aed", size = 5),
                           text = hover, hovertemplate = paste0("%{text}<br>", label_x, ": %{z:.3f}<extra>Projected</extra>"))
  }
  p <- plotly::layout(p, uirevision = "analysis-camera",
                      scene = list(
                              xaxis = list(title = da_axis_label(var_names[2], standardised), range = ar),
      yaxis = list(title = da_axis_label(var_names[3], standardised), range = br),
      zaxis = list(title = da_axis_label(var_names[1], standardised), range = zr)),
                      margin = list(l = 0, r = 0, b = 0, t = 20))
  plotly::config(p, displaylogo = FALSE)
}

dataAnalysisServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    data_source <- shiny::reactiveVal("upload")
    status <- shiny::reactiveVal("Upload a CSV to begin.")
    output$status <- shiny::renderText(status())
    
    uploaded <- shiny::reactive({
      if (identical(data_source(), "demo")) {
        shiny::validate(shiny::need(file.exists(DA_DEMO_PATH),
                                    paste0("Demo file not found: ", DA_DEMO_PATH)))
        d <- utils::read.csv(DA_DEMO_PATH, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        shiny::req(input$file)
        shiny::validate(shiny::need(input$file$size <= 5 * 1024^2,
                                    "This prototype accepts files up to 5 MB."))
        d <- tryCatch(utils::read.csv(input$file$datapath,
                                      stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
        shiny::validate(shiny::need(!inherits(d, "error"), "Unable to read this CSV."))
      }
      shiny::validate(shiny::need(nrow(d) > 0, "The CSV has no data rows."),
                      shiny::need(!anyDuplicated(names(d)), "Column names must be unique."))
      d
    })
    
    shiny::observeEvent(input$file, {
      data_source("upload")
      result(NULL)
      status("Dataset changed. Choose variables and fit again.")
    })
    
    shiny::observeEvent(input$use_demo, {
      data_source("demo")
      result(NULL)
      status("Demo dataset loaded. Click Fit cusp model.")
    }, ignoreInit = TRUE)
    
    output$variables <- shiny::renderUI({
      d <- uploaded()
      numeric_columns <- names(d)[vapply(d, is.numeric, logical(1))]
      shiny::validate(shiny::need(length(numeric_columns) >= 2,
                                  "Need a numeric response and at least one separate numeric predictor."))
      ns <- session$ns
      pick <- function(name, fallback) if (name %in% numeric_columns) name else fallback
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("response"), "Continuous response", numeric_columns,
                                            selected = pick("attitude", numeric_columns[1]))),
        shiny::column(4, shiny::selectInput(ns("normal"), "Predictor of normal control (alpha)", numeric_columns,
                                            selected = pick("information", numeric_columns[2]))),
        shiny::column(4, shiny::selectInput(ns("splitting"), "Predictor of splitting control (beta)", numeric_columns,
                                            selected = pick("involvement", numeric_columns[min(3, length(numeric_columns))])))
      )
    })
    
    shiny::observeEvent(list(input$response, input$normal, input$splitting), {
      result(NULL)
      status("Variable selection changed. Click Fit cusp model.")
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$fit, {
      result(NULL)
      shiny::req(input$response, input$normal, input$splitting)
      answer <- tryCatch({
        d <- uploaded()
        if (!requireNamespace("cusp", quietly = TRUE))
          stop('Install the package in your R console: install.packages("cusp")')
        if (input$response %in% c(input$normal, input$splitting))
          stop("The response cannot also be a control predictor.")
        selected <- c(input$response, input$normal, input$splitting)
        if (!all(selected %in% names(d))) stop("Select valid columns.")
        ok <- Reduce(`&`, lapply(d[selected], is.finite))
        if (sum(ok) < 30)
          stop("This prototype requires at least 30 complete rows. This is a guardrail, not a guarantee of adequate sample size.")
        dat <- data.frame(response = d[[input$response]][ok],
                          normal = d[[input$normal]][ok], splitting = d[[input$splitting]][ok])
        if (length(unique(dat$response)) <= 2)
          stop("The response must be continuous; binary or constant responses are not supported.")
        if (sd(dat$normal) == 0 || sd(dat$splitting) == 0)
          stop("Both selected control predictors must vary across respondents.")
        
        # Standardising makes inputs with different units easier to fit.
        scaling <- data.frame(variable = selected,
                              role = c("response", "normal", "splitting"),
                              mean = vapply(dat, mean, numeric(1)), sd = vapply(dat, sd, numeric(1)))
        scaled <- as.data.frame(lapply(dat, function(v) as.numeric(scale(v))))
        warnings <- character()
        fit <- shiny::withProgress(message = "Fitting cusp model", value = 0.5, {
          withCallingHandlers(
            cusp::cusp(y ~ response, alpha ~ normal, beta ~ splitting, data = scaled),
            warning = function(w) {
              warnings <<- c(warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            })
        })
        if (!isTRUE(fit$converged))
          stop(paste("Optimisation did not converge. Do not interpret this fit.", fit$message))
        eta <- as.matrix(fit$linear.predictors)
        state <- as.numeric(fit$y)
        if (!all(c("alpha", "beta") %in% colnames(eta)) ||
            nrow(eta) != nrow(dat) || length(state) != nrow(dat))
          stop("Unexpected cusp object structure; check your package version.")
        if (!all(is.finite(c(eta, state, fit$coefficients, fit$aic))))
          stop("Fit returned non-finite values. Inspect the data and model.")
        coordinates <- data.frame(row = which(ok), response = dat$response,
                                  alpha = eta[, "alpha"], beta = eta[, "beta"], state = state)
        coordinates$stable_projection <- mapply(da_nearest_stable,
                                                coordinates$alpha, coordinates$beta, coordinates$state)
        coordinates$projection_residual <- coordinates$state - coordinates$stable_projection
        coordinates$inside_bistable_region <- with(coordinates,
                                                   beta > 0 & 4 * beta^3 - 27 * alpha^2 > 0)
        
        # Standard errors may be unavailable if the Hessian is ill-conditioned.
        se <- tryCatch(sqrt(diag(stats::vcov(fit))), error = function(e) {
          warnings <<- c(warnings, "Standard errors unavailable: Hessian could not be inverted.")
          rep(NA_real_, length(fit$coefficients))
        })
        hessian_ok <- tryCatch(all(eigen(fit$Hessian, symmetric = TRUE,
                                         only.values = TRUE)$values > 0), error = function(e) FALSE)
        if (!hessian_ok) warnings <- c(warnings,
                                       "Hessian is not positive definite. Parameter uncertainty/identification may be unreliable.")
        if (length(unique(dat$response)) < 10) warnings <- c(warnings,
                                                             "Response has fewer than 10 distinct values: assess whether a continuous density model is appropriate.")
        coefficients <- data.frame(parameter = names(fit$coefficients),
                                   estimate = unname(fit$coefficients), standard_error = unname(se))
        # Linear benchmark on the same standardised data.
        # Base R's AIC/BIC count the error variance as a parameter for lm.
        linear_fit <- stats::lm(response ~ normal + splitting, data = scaled)
        n_obs <- nrow(scaled)
        k_cusp <- length(fit$coefficients)
        cusp_bic <- fit$aic + k_cusp * (log(n_obs) - 2)  # BIC derived from the package AIC
        comparison <- data.frame(
          model = c("Cusp", "Linear regression"),
          parameters = c(k_cusp, length(stats::coef(linear_fit)) + 1L),
          AIC = c(fit$aic, stats::AIC(linear_fit)),
          BIC = c(cusp_bic, stats::BIC(linear_fit)))
        comparison$delta_AIC <- comparison$AIC - min(comparison$AIC)
        comparison$delta_BIC <- comparison$BIC - min(comparison$BIC)
        list(fit = fit, coordinates = coordinates, coefficients = coefficients,comparison = comparison,
             scaling = scaling, excluded = sum(!ok), warnings = unique(warnings),
             columns = selected)
      }, error = function(e) e)
      if (inherits(answer, "error")) {
        message_text <- conditionMessage(answer)
        if (!nzchar(message_text)) message_text <- "Upload a valid CSV file first."
        status(paste("Fit stopped:", message_text))
      } else {
        result(answer)
        status(paste0("Fit completed using ", nrow(answer$coordinates), " rows; ",
                      answer$excluded, " excluded. Optimiser converged.\n",
                      "Columns: response = ", answer$columns[1], "; alpha predictor = ",
                      answer$columns[2], "; beta predictor = ", answer$columns[3], ".\n",
                      "Input columns were standardised; estimates refer to those scaled inputs.\n",
                      paste(answer$warnings, collapse = "\n")))
      }
    })
    
    output$coefficients <- shiny::renderTable({
      shiny::req(result())
      result()$coefficients
    }, digits = 4)
    
    output$metrics <- shiny::renderTable({
      r <- result(); shiny::req(r)
      data.frame(measure = c("Complete observations", "Excluded rows", "Package AIC",
                             "Estimated controls inside bistable region (%)"),
                 value = c(nrow(r$coordinates), r$excluded, r$fit$aic,
                           100 * mean(r$coordinates$inside_bistable_region)))
    }, digits = 3)
    
    output$comparison <- shiny::renderTable({
      shiny::req(result())
      result()$comparison
    }, digits = 2)
    
    output$surface <- plotly::renderPlotly({
      r <- result(); shiny::req(r)
      da_plot(r$coordinates, isTRUE(input$observed), isTRUE(input$projected), r$columns)
      })
    
    output$coordinates <- shiny::renderTable({
      shiny::req(result())
      head(result()$coordinates, 5)
    }, digits = 3)
    
    output$download <- shiny::downloadHandler(
      filename = function() "cusp_respondent_coordinates.csv",
      content = function(file) {
        shiny::req(result())
        utils::write.csv(result()$coordinates, file, row.names = FALSE)
      }
    )
    output$download_all <- shiny::downloadHandler(
      filename = function() {
        paste0("cusp_analysis_", Sys.Date(), ".zip")
      },
      content = function(file) {
        r <- result()
        shiny::req(r, r$comparison)
        
        if (!requireNamespace("zip", quietly = TRUE)) {
          stop('Install the package in your R console: install.packages("zip")')
        }
        
        # Create a temporary folder for the exported tables.
        export_dir <- tempfile("cusp_export_")
        dir.create(export_dir)
        
        # Remove the temporary folder when the download is finished.
        on.exit(
          unlink(export_dir, recursive = TRUE),
          add = TRUE
        )
        
        write_table <- function(table, filename) {
          utils::write.csv(
            table,
            file = file.path(export_dir, filename),
            row.names = FALSE,
            na = ""
          )
        }
        
        # 1. Fit information.
        metrics <- data.frame(
          measure = c(
            "Complete observations",
            "Excluded rows",
            "Package AIC",
            "Estimated controls inside bistable region (%)"
          ),
          value = c(
            nrow(r$coordinates),
            r$excluded,
            r$fit$aic,
            100 * mean(r$coordinates$inside_bistable_region)
          )
        )
       
        # 2. PDF with results
        
        if (!requireNamespace("gridExtra", quietly = TRUE)) {
          stop('Install the package in your R console: install.packages("gridExtra")')
        }
        
        # Format numbers for the report only.
        # The coordinates CSV keeps its underlying numerical precision.
        format_table <- function(table) {
          table[] <- lapply(table, function(column) {
            if (is.numeric(column)) {
              ifelse(
                is.na(column),
                "Unavailable",
                formatC(column, format = "f", digits = 4)
              )
            } else {
              as.character(column)
            }
          })
          table
        }
        
        estimates <- format_table(r$coefficients)
        names(estimates) <- c(
          "Parameter", "Estimate", "Standard error"
        )
        
        fit_information <- format_table(metrics)
        names(fit_information) <- c("Measure", "Value")
        
        comparison <- format_table(r$comparison)
        names(comparison) <- c(
          "Model", "Parameters", "AIC", "BIC",
          "Delta AIC", "Delta BIC"
        )
        
        # Show sample sizes and parameter counts as whole numbers.
        fit_information$Value[1:2] <- as.character(metrics$value[1:2])
        comparison$Parameters <- as.character(r$comparison$parameters)
        
        report_tables <- list(
          "Parameter estimates and standard errors" = estimates,
          "Fit information" = fit_information,
          "Comparison with linear regression" = comparison
        )
        
        notes <- c(
          "Estimates refer to standardised inputs. Unavailable standard errors are marked.",
          "Bistable-region membership is descriptive; it does not establish hysteresis.",
          "Lower AIC/BIC is better. Scores are exported as calculated by the app."
        )
        
        # Open one PDF and write one page
        # Put all three report sections on one A4 portrait page.
        grDevices::pdf(
          file = file.path(export_dir, "analysis_report.pdf"),
          width = 8.3,
          height = 11.7,
          family = "Helvetica"
        )
        
        tryCatch({
          heading <- grid::textGrob(
            "Cusp model analysis",
            x = 0,
            hjust = 0,
            gp = grid::gpar(
              fontsize = 20,
              fontface = "bold",
              col = "#172033"
            )
          )
          
          subtitle <- grid::textGrob(
            paste0(
              "Response: ", r$columns[1],
              "\nAlpha predictor: ", r$columns[2],
              " | Beta predictor: ", r$columns[3],
              "\nExported: ", Sys.Date()
            ),
            x = 0,
            hjust = 0,
            gp = grid::gpar(
              fontsize = 10,
              col = "#526176"
            )
          )
          
          make_section <- function(title, data) {
            section_heading <- grid::textGrob(
              title,
              x = 0,
              hjust = 0,
              gp = grid::gpar(
                fontsize = 13,
                fontface = "bold",
                col = "#172033"
              )
            )
            
            table <- gridExtra::tableGrob(
              data,
              rows = NULL,
              theme = gridExtra::ttheme_minimal(
                base_size = 9,
                padding = grid::unit(c(3, 3), "mm"),
                core = list(
                  fg_params = list(col = "#172033"),
                  bg_params = list(
                    fill = c("#f1f5f9", "#ffffff"),
                    col = NA
                  )
                ),
                colhead = list(
                  fg_params = list(
                    col = "#ffffff",
                    fontface = "bold"
                  ),
                  bg_params = list(
                    fill = "#334155",
                    col = NA
                  )
                )
              )
            )
            
            gridExtra::arrangeGrob(
              section_heading,
              table,
              ncol = 1,
              heights = grid::unit.c(
                grid::unit(9, "mm"),
                sum(table$heights)
              )
            )
          }
          
          sections <- lapply(
            seq_along(report_tables),
            function(i) {
              make_section(
                names(report_tables)[i],
                report_tables[[i]]
              )
            }
          )
          
          footer <- grid::textGrob(
            paste(
              "Estimates refer to standardised inputs.",
              "Unavailable standard errors are marked.",
              "Lower AIC/BIC is better; values are exported as calculated by the app.",
              "Bistable-region membership does not establish hysteresis.",
              sep = "\n"
            ),
            x = 0,
            hjust = 0,
            gp = grid::gpar(
              fontsize = 9,
              col = "#526176"
            )
          )
          
          page <- gridExtra::arrangeGrob(
            heading,
            subtitle,
            sections[[1]],
            sections[[2]],
            sections[[3]],
            footer,
            ncol = 1,
            heights = c(0.07, 0.10, 0.29, 0.23, 0.20, 0.11)
          )
          
          grid::grid.newpage()
          grid::pushViewport(
            grid::viewport(width = 0.90, height = 0.92)
          )
          grid::grid.draw(page)
          grid::popViewport()
        }, finally = {
          grDevices::dev.off()
        })
        
        # 3. All fitted respondent coordinates.
        write_table(
          r$coordinates,
          "respondent_coordinates.csv"
        )
        
        # Bundle the four files into a ZIP.
        zip::zipr(
          zipfile = file,
          files = list.files(export_dir),
          root = export_dir
        )
      }
    )
    invisible(result)
  })
}

                                
#----------base-model update part----------
#re-load ui
ui <- fluidPage(
  tags$head(tags$style(HTML("body { background: #f8fafc; color: #172033; }
    .well { background: white; border-color: #e2e8f0; }
    .note { color: #526176; font-size: 13px; }"))),
  titlePanel("Interactive Cusp Catastrophe Tool: Visualisation & Data-Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "person", "Person to move",
        choices = c("Select a person" = "", "Person 1" = "1"),
        selected = "1", multiple = FALSE
      ),
      uiOutput("controls"), uiOutput("selection_note"),
      actionButton("add_person", "Add person"),
      actionButton("remove_people", "Remove selected person", class = "btn-danger"),
      p(class = "note", "Removing a person permanently deletes them from the plot and list. Switching selection does not delete anyone. New people are assigned the lowest available number."),
      actionButton("reset", "Reset everyone's controls"), hr(),
      numericInput("speed", "Control movement speed (units / second)", value = 1, min = 0.1, max = 5, step = 0.1),
      strong("Selected person's position"), verbatimTextOutput("coordinates"),
      actionButton("switch_branch", "Switch selected person's branch"),
      p(class = "note", "Switching acts at the selected person's current controls when two stable equilibria exist. Other people remain unchanged."), hr(),
      strong("Rename axes"),
      textInput("axis_normal", "Normal-control axis name", default_labels$a),
      textInput("axis_splitting", "Splitting-control axis name", default_labels$b),
      textInput("axis_response", "Behavioural axis name", default_labels$response),
      p(class = "note", "Use this option to make your theory-driven visualisations easier to interpret. Changing names only changes the labels."), hr(),
      h4("We need to talk about the cusp…"),
      p(class = "note", "The surface of the cusp is divided into two sections: stable and unstable equilibria (in different colours). The latter is also referred to as the inaccessible region as people can only occupy these positions momentarily. The dark lines mark the fold."),
      p(class = "note", "At a fold each person jumps directly to the other stable branch. We hope that this app helps you visualise psychological phenomena whose dynamics theory implies is cusp-like. One such example is the cusp model of attitudes (van der Maas et al., 2003)."),
      strong("Reference"),
      p(class = "note", "van der Maas, H. L. J., Kolstein, R., & van der Pligt, J. (2003). Sudden transitions in attitudes. ",
        tags$em("Sociological Methods & Research, 32"), "(2), 125–152. ",
        tags$a(href = "https://doi.org/10.1177/0049124103253773",
               "https://doi.org/10.1177/0049124103253773", target = "_blank"))),
    mainPanel(plotlyOutput("cusp", height = "720px"),
              p(class = "note", "Model: V(x; a, b) = x⁴/4 − bx²/2 − ax. Equilibrium: x³ − bx − a = 0. Stability: 3x² − b > 0. Motion uses quasi-static stable-branch continuation, not physical time-domain dynamics."),
              dataAnalysisUI("analysis"))))
#end

#re-load server function with new data-analysis piece
server <- function(input, output, session) {
  # Create fresh state and helpers for each browser session.
  state <- create_person_state()
  helpers <- create_server_helpers(input, session, state)
  
  setup_selection(input, session, state, helpers)
  setup_controls(input, output, session, state, helpers)
  setup_person_management(input, session, state, helpers)
  setup_movement(input, session, state)
  setup_readout(output, state, helpers)
  setup_plot(output, state, helpers)
  
  dataAnalysisServer("analysis")
}
#end
#----------end of base_model update----------
