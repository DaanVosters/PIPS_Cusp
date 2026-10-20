dataAnalysisUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::hr(),
    shiny::h3("Fit a cusp model to uploaded data"),
    shiny::p("Upload independent observations with a continuous response. Choose numeric predictors for the normal and splitting controls. Binary responses are not supported."),
    shiny::fileInput(ns("file"), "CSV dataset", accept = ".csv"),
    shiny::p("Use commas between columns and decimal points for numbers. Missing or non-finite values in selected variables are excluded."),
    shiny::uiOutput(ns("variables")),
    shiny::actionButton(ns("fit"), "Fit cusp model", class = "btn-primary"),
    shiny::verbatimTextOutput(ns("status")),
    shiny::h4("Parameter estimates"),
    shiny::tableOutput(ns("coefficients")),
    shiny::h4("Fit information"),
    shiny::tableOutput(ns("metrics")),
    shiny::checkboxInput(ns("observed"), "Show transformed observed responses", TRUE),
    shiny::checkboxInput(ns("projected"), "Show projections onto stable equilibria", TRUE),
    plotly::plotlyOutput(ns("surface"), height = "600px"),
    shiny::p("Grey dots are transformed observations and need not lie on the surface. Purple dots place each observation on its nearest stable equilibrium, keeping its estimated controls fixed. This uses the observed response to choose a branch: it is a descriptive projection, not an independent prediction or evidence of hysteresis."),
    shiny::h4("Respondent coordinates (first 20 rows)"),
    shiny::tableOutput(ns("coordinates")),
    shiny::downloadButton(ns("download"), "Download all fitted coordinates"),
    shiny::p("A successful fit does not prove a catastrophe model is correct. Check theory, alternative models, convergence and sensitivity to starting values. This first version fits one numeric predictor per control and uses one optimisation start.")
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

# Builds the 3D plot: cusp surface, fold curve, and respondent points.
da_plot <- function(points, show_observed, show_projected) {
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
                           hovertemplate = "alpha: %{x:.3f}<br>beta: %{y:.3f}<br>state: %{z:.3f}<extra>Stable</extra>")
  p <- plotly::add_surface(p, x = A, y = B, z = unstable,
                           name = "Unstable equilibria", opacity = 0.25, showscale = FALSE,
                           colorscale = list(c(0, "#fdba74"), c(1, "#fdba74")),
                           hovertemplate = "alpha: %{x:.3f}<br>beta: %{y:.3f}<br>state: %{z:.3f}<extra>Unstable</extra>")
  
  # Fold curve: alpha = -2 x^3, beta = 3 x^2
  f <- seq(-sqrt(max(br[2], 0) / 3), sqrt(max(br[2], 0) / 3), length.out = 400)
  fa <- -2 * f^3
  fb <- 3 * f^2
  keep <- fa >= ar[1] & fa <= ar[2] & fb >= br[1] & fb <= br[2]
  f[!keep] <- NA_real_
  p <- plotly::add_trace(p, x = fa, y = fb, z = f,
                         type = "scatter3d", mode = "lines", name = "Folds",
                         line = list(color = "#334155", width = 5), hoverinfo = "skip")
  
  hover <- paste0("CSV row ", points$row, "<br>Raw response: ",
                  signif(points$response, 4), "<br>alpha: ", signif(points$alpha, 4),
                  "<br>beta: ", signif(points$beta, 4))
  if (show_observed) {
    p <- plotly::add_trace(p, x = points$alpha, y = points$beta,
                           z = points$state, type = "scatter3d", mode = "markers",
                           name = "Observed (transformed)", marker = list(color = "#475569", size = 4),
                           text = hover, hovertemplate = "%{text}<br>state: %{z:.3f}<extra>Observed</extra>")
  }
  if (show_projected) {
    p <- plotly::add_trace(p, x = points$alpha, y = points$beta,
                           z = points$stable_projection, type = "scatter3d", mode = "markers",
                           name = "Nearest stable projection", marker = list(color = "#7c3aed", size = 5),
                           text = hover, hovertemplate = "%{text}<br>state: %{z:.3f}<extra>Projected</extra>")
  }
  p <- plotly::layout(p, uirevision = "analysis-camera",
                      scene = list(
                        xaxis = list(title = "Estimated normal control (alpha)", range = ar),
                        yaxis = list(title = "Estimated splitting control (beta)", range = br),
                        zaxis = list(title = "Canonical response state", range = zr)),
                      margin = list(l = 0, r = 0, b = 0, t = 20))
  plotly::config(p, displaylogo = FALSE)
}

dataAnalysisServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("Upload a CSV to begin.")
    output$status <- shiny::renderText(status())
    
    uploaded <- shiny::reactive({
      shiny::req(input$file)
      shiny::validate(shiny::need(input$file$size <= 5 * 1024^2,
                                  "This prototype accepts files up to 5 MB."))
      d <- tryCatch(utils::read.csv(input$file$datapath,
                                    stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
      shiny::validate(shiny::need(!inherits(d, "error"), "Unable to read this CSV."))
      shiny::validate(shiny::need(nrow(d) > 0, "The CSV has no data rows."),
                      shiny::need(!anyDuplicated(names(d)), "Column names must be unique."))
      d
    })
    
    shiny::observeEvent(input$file, {
      result(NULL)
      status("Dataset changed. Choose variables and fit again.")
    })
    
    output$variables <- shiny::renderUI({
      d <- uploaded()
      numeric_columns <- names(d)[vapply(d, is.numeric, logical(1))]
      shiny::validate(shiny::need(length(numeric_columns) >= 2,
                                  "Need a numeric response and at least one separate numeric predictor."))
      ns <- session$ns
      shiny::tagList(
        shiny::selectInput(ns("response"), "Continuous response", numeric_columns,
                           selected = numeric_columns[1]),
        shiny::selectInput(ns("normal"), "Predictor of normal control (alpha)", numeric_columns,
                           selected = numeric_columns[2]),
        shiny::selectInput(ns("splitting"), "Predictor of splitting control (beta)", numeric_columns,
                           selected = numeric_columns[min(3, length(numeric_columns))])
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
        list(fit = fit, coordinates = coordinates, coefficients = coefficients,
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
                      "a[...] = normal-control coefficients; b[...] = splitting-control coefficients;\n",
                      "w[...] = response-to-canonical-state transformation coefficients.\n",
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
    
    output$surface <- plotly::renderPlotly({
      r <- result(); shiny::req(r)
      da_plot(r$coordinates, isTRUE(input$observed), isTRUE(input$projected))
    })
    
    output$coordinates <- shiny::renderTable({
      shiny::req(result())
      head(result()$coordinates, 20)
    }, digits = 3)
    
    output$download <- shiny::downloadHandler(
      filename = function() "cusp_respondent_coordinates.csv",
      content = function(file) {
        shiny::req(result())
        utils::write.csv(result()$coordinates, file, row.names = FALSE)
      }
    )
    invisible(result)
  })
}

