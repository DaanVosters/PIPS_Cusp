library(shiny)
library(plotly)

#----------Adjustable parameters----------
normal_limits <- c(-3, 3)
splitting_limits <- c(-1, 3)
default_labels <- list(a = "Normal axis (a)", b = "Splitting axis (b)", response = "Response (x)")
frame_ms <- 25 #framerate, lower = smoother but more demanding
max_control_step <- 0.005 #smaller value should lead to better tracking over the surface according to miss AI
#----------end----------

#----------Figure text----------                   
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
              p(class = "note", "Model: V(x; a, b) = x⁴/4 − bx²/2 − ax. Equilibrium: x³ − bx − a = 0. Stability: 3x² − b > 0. Motion uses quasi-static stable-branch continuation, not physical time-domain dynamics."))))
#----------end----------


#----------Important functions I can understand----------
random_colour <- function() {
  return(hsv(runif(1), 1, 1)) #random hue, fully saturated, and full brightness
} 

# Find the next lowest available person number
next_available_number <- function(ids) {
  used <- as.integer(ids)
  number <- 1L
  while (number %in% used) number <- number+1L
  return(number)
}

# Function for creating the person selection list; keeps the person numbers in the drop-down menu ordered from low to high.
person_choices <- function(records) {
  index <- order(as.integer(records$id))
  return(setNames(records$id[index], records$name[index]))
}

# For finding the equilibria of the cusp, for a given a and b parameters.
real_roots <- function(a, b) {
  roots <- polyroot(c(-a, -b, 0, 1))
  return(sort(Re(roots[abs(Im(roots)) < 1e-6]))) #Imaginary part need to stay otherwise more roots are found (I think)
}

# Returns next position, always at a stable equilibrium, for a person
choose_equilibrium <- function(a, b, previous_x) {
  roots <- real_roots(a, b)
  candidates <- roots[3*roots^2-b >= -1e-7] #if second derivative at roots is bigger than 0...; slightly negative to allow for rounding error according to AI.
  return(candidates[which.min(abs(candidates - previous_x))])
}

# Random starting point for a new person: random controls inside the slider
# limits, placed on a randomly chosen stable branch.
random_start <- function() {
  for (attempt in seq_len(100L)) {
    a <- round(runif(1, normal_limits[1], normal_limits[2]), 2)
    b <- round(runif(1, splitting_limits[1], splitting_limits[2]), 2)
    roots <- real_roots(a, b)
    stable <- roots[3 * roots^2 - b > 1e-7]
    
    if (length(stable)) {
      x <- stable[sample.int(length(stable), 1L)]
      return(list(a = a, b = b, x = x))
    }
  }
  
  stop("Could not generate a stable random starting point.")
}

#for drawing 3d persons
figure_geometry <- function(a, b, x) {
  h <- 0.30 #person height
  w <- 0.09 #person width
  return(list(a = c(a, a, NA, a-w, a+w, NA, a, a-w, NA, a, a+w), b = rep(b, 11),
              x = c(x+h*0.82, x+h*0.35, NA, x+h*0.62, x+h*0.62,
                    NA, x+h*0.35, x, NA, x+h*0.35, x), head_x = x+h))
}
person_traces <- function(person, position, selected = FALSE, labels = default_labels) {
  a <- position$a
  b <- position$b
  x <- position$x
  g <- figure_geometry(a, b, x)
  return(list(
    list(type = "scatter3d", mode = "lines", x = g$a, y = g$b, z = g$x,
         line = list(color = person$color, width = if (selected) 8 else 5),
         name = person$name, showlegend = FALSE, hoverinfo = "skip"),
    list(type = "scatter3d", mode = "markers+text", x = list(a), y = list(b), z = list(g$head_x),
         marker = list(color = person$color, size = if (selected) 7 else 5),
         text = list(person$name), textposition = "top center",
         textfont = list(color = person$color, size = 12), name = person$name,
         showlegend = FALSE, hovertemplate = paste0(person$name, "<extra></extra>")),
    list(type = "scatter3d", mode = "markers", x = list(a), y = list(b), z = list(x),
         marker = list(color = person$color, size = 4), name = person$name,
         showlegend = FALSE, hovertemplate = anchor_hover(person$name, labels))))
}
#end


#according to AI this is for the text on hovering over the cusp
hover_coordinates <- function(labels) {
  safe <- lapply(labels, function(s) as.character(htmltools::htmlEscape(s)))
  return(paste0(safe$a, ": %{x:.3f}<br>", safe$b, ": %{y:.3f}<br>", safe$response, ": %{z:.3f}"))
}
#and this for hovering over persons
anchor_hover <- function(name, labels) {
  return(paste0(as.character(htmltools::htmlEscape(name)), "<br>", hover_coordinates(labels), "<extra></extra>"))
}
#----------end----------


#----------Setup figure planes and line----------
x_grid <- seq(normal_limits[1] - .1, normal_limits[2] + .1, length.out = 100) #length.out values here and next line very important for framerate
b_grid <- seq(splitting_limits[1], splitting_limits[2], length.out = 66)
A <- outer(x_grid, b_grid, function(x, b) x^3-b*x) #required A's to make the first derivative = 0
B <- matrix(rep(b_grid, each = length(x_grid)), nrow = length(x_grid))
X <- matrix(rep(x_grid, times = length(b_grid)), nrow = length(x_grid))
stability <- 3*X^2-B #second derivative of the cusp function; checks whether a certain x value is stable or unstable for a given b.
X_stable <- X
X_unstable <- X
X_stable[stability <= 0] <- NA_real_ 
X_unstable[stability >= 0] <- NA_real_

#For black line separating stable from unstable plane
fold_x <- seq(-1, 1, length.out = 300)
fold_a <- -2*fold_x^3
fold_b <- 3*fold_x^2
#----------end----------


#----------AI code I don't understand----------

# 1. Create shared state
create_person_state <- function() {
  initial_targets <- data.frame(
    id = "1", name = "Person 1", a = -1, b = 2,
    color = random_colour(), stringsAsFactors = FALSE
  )
  
  initial_positions <- data.frame(
    id = "1", a = -1, b = 2,
    x = choose_equilibrium(-1, 2, -2), stringsAsFactors = FALSE
  )
  
  # A list groups the reactive objects; it does not merge their dependencies.
  # Read with state$person_targets().
  # Write with state$person_targets(updated_targets).
  #
  # Targets and positions retain Plotly trace order, not numerical ID order.
  # Only the dropdown choices are sorted numerically.
  list(
    initial_targets = initial_targets,
    initial_positions = initial_positions,
    person_targets = reactiveVal(initial_targets),
    person_positions = reactiveVal(initial_positions),
    person_ids = reactiveVal("1"),
    selected_people = reactiveVal("1"),
    controls_version = reactiveVal(0L),
    previous_slider_values = reactiveVal(NULL)
  )
}


# 2. Shared server utilities
create_server_helpers <- function(input, session, state) {
  # Update the existing plot rather than rebuilding it, preserving the camera.
  update_plot <- function(method, ...) {
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, method, ...)
  }
  
  # Plotly uses zero-based trace indices.
  # Traces 0, 1, and 2 are the two sheets and the folds.
  # Each person then has three traces: body, head, and foot marker.
  body_indices <- function(rows) {
    3L + 3L * (rows - 1L)
  }
  
  all_person_trace_indices <- function(rows) {
    first <- body_indices(rows)
    as.vector(rbind(first, first + 1L, first + 2L))
  }
  
  refresh_person_choices <- function(targets, selected) {
    selected <- as.character(selected)
    selected <- selected[selected %in% targets$id]
    
    updateSelectInput(
      session, "person",
      choices = c("Select a person" = "", person_choices(targets)),
      selected = if (length(selected)) selected[1] else ""
    )
  }
  
  # Changing this counter gives the sliders fresh input IDs.
  # Old slider values therefore cannot affect the new selection.
  refresh_controls <- function() {
    state$controls_version(isolate(state$controls_version()) + 1L)
  }
  
  use_default_if_blank <- function(value, default) {
    if (is.null(value) || !nzchar(trimws(value))) {
      default
    } else {
      trimws(value)
    }
  }
  
  # Controls, the plot, and the readout share these same reactive labels.
  axis_labels <- debounce(reactive({
    list(
      a = use_default_if_blank(input$axis_normal, default_labels$a),
      b = use_default_if_blank(input$axis_splitting, default_labels$b),
      response = use_default_if_blank(
        input$axis_response, default_labels$response
      )
    )
  }), 250)
  
  list(
    update_plot = update_plot,
    body_indices = body_indices,
    all_person_trace_indices = all_person_trace_indices,
    refresh_person_choices = refresh_person_choices,
    refresh_controls = refresh_controls,
    axis_labels = axis_labels
  )
}


# 3. Selection
setup_selection <- function(input, session, state, helpers) {
  # Observe membership, not changes to people's target coordinates.
  observeEvent(list(input$person, state$person_ids()), {
    requested_ids <- as.character(input$person)
    valid_ids <- requested_ids[requested_ids %in% state$person_ids()]
    
    if (length(valid_ids) > 1L) {
      valid_ids <- valid_ids[1]
    }
    
    if (!identical(valid_ids, state$selected_people())) {
      state$selected_people(valid_ids)
      helpers$refresh_controls()
    }
  }, priority = 100)
}


# 4. Shared sliders
setup_controls <- function(input, output, session, state, helpers) {
  # Loading slider values after selection/reset must not change targets.
  # isolate() prevents position updates from rebuilding these sliders.
  slider_setup <- reactive({
    selected <- state$selected_people()
    version <- state$controls_version()
    req(length(selected) == 1L)
    
    positions <- isolate(state$person_positions())
    reference_row <- match(selected[1], positions$id)
    req(!is.na(reference_row))
    
    list(
      version = version,
      a_input = paste0("normal_", version),
      b_input = paste0("splitting_", version),
      initial_a = round(positions$a[reference_row], 2),
      initial_b = round(positions$b[reference_row], 2)
    )
  })
  
  output$controls <- renderUI({
    if (!length(state$selected_people())) {
      return(p("Select a person to show the sliders."))
    }
    
    setup <- slider_setup()
    labels <- isolate(helpers$axis_labels())
    
    tagList(
      sliderInput(
        setup$a_input, paste0(labels$a, " — set target"),
        min = normal_limits[1], max = normal_limits[2],
        value = setup$initial_a, step = 0.01
      ),
      sliderInput(
        setup$b_input, paste0(labels$b, " — set target"),
        min = splitting_limits[1], max = splitting_limits[2],
        value = setup$initial_b, step = 0.01
      )
    )
  })
  
  output$selection_note <- renderUI({
    selected <- state$selected_people()
    if (!length(selected)) return(NULL)
    
    targets <- isolate(state$person_targets())
    reference_name <- targets$name[match(selected[1], targets$id)]
    
    p(class = "note", paste0(
      reference_name, " is selected. Their position is displayed on the sliders, ",
      "rounded to 0.01. Using the sliders, you can move the selected person ",
      "along the cusp."
    ))
  })
  
  # Rename the existing sliders without resetting their values.
  observeEvent(list(helpers$axis_labels(), slider_setup()), {
    labels <- helpers$axis_labels()
    setup <- slider_setup()
    
    updateSliderInput(
      session, setup$a_input,
      label = paste0(labels$a, " — set target")
    )
    updateSliderInput(
      session, setup$b_input,
      label = paste0(labels$b, " — set target")
    )
  })
  
  # Apply only the slider that actually changed to the selected targets.
  observe({
    setup <- slider_setup()
    new_a <- input[[setup$a_input]]
    new_b <- input[[setup$b_input]]
    req(!is.null(new_a), !is.null(new_b))
    
    previous <- isolate(state$previous_slider_values())
    
    if (is.null(previous) || previous$version != setup$version) {
      previous <- list(
        version = setup$version,
        a = setup$initial_a, b = setup$initial_b
      )
    }
    
    a_changed <- !isTRUE(all.equal(new_a, previous$a))
    b_changed <- !isTRUE(all.equal(new_b, previous$b))
    
    state$previous_slider_values(list(
      version = setup$version, a = new_a, b = new_b
    ))
    if (!a_changed && !b_changed) return()
    
    targets <- isolate(state$person_targets())
    selected_rows <- targets$id %in% isolate(state$selected_people())
    
    if (a_changed) targets$a[selected_rows] <- new_a
    if (b_changed) targets$b[selected_rows] <- new_b
    
    state$person_targets(targets)
  })
}


# 5. Add, remove, reset, and switch branches
setup_person_management <- function(input, session, state, helpers) {
  observeEvent(input$add_person, {
    targets <- state$person_targets()
    positions <- state$person_positions()
    new_id <- as.character(next_available_number(targets$id))
    
    start <- random_start()
    new_target <- data.frame(
      id = new_id,
      name = paste("Person", new_id),
      a = start$a,
      b = start$b,
      color = random_colour(),
      stringsAsFactors = FALSE
    )
    new_position <- data.frame(
      id = new_id,
      a = start$a,
      b = start$b,
      x = start$x,
      stringsAsFactors = FALSE
    )
    
    # Add traces before queuing updates that use their new indices.
    helpers$update_plot("addTraces", person_traces(
      new_target, new_position, selected = TRUE,
      labels = helpers$axis_labels()
    ))
    
    targets <- rbind(targets, new_target)
    state$person_targets(targets)
    state$person_positions(rbind(positions, new_position))
    state$person_ids(targets$id)
    
    helpers$refresh_person_choices(targets, new_id)
  })
  
  observeEvent(input$remove_people, {
    selected <- state$selected_people()
    if (!length(selected)) {
      showNotification("Select a person to remove.", type = "message")
      return()
    }
    
    targets <- state$person_targets()
    positions <- state$person_positions()
    removed_rows <- which(targets$id %in% selected)
    if (!length(removed_rows)) return()
    
    # Surviving traces compact to match the remaining table rows.
    removed_traces <- helpers$all_person_trace_indices(removed_rows)
    helpers$update_plot("deleteTraces", sort(removed_traces, decreasing = TRUE))
    
    targets <- targets[!(targets$id %in% selected), , drop = FALSE]
    positions <- positions[!(positions$id %in% selected), , drop = FALSE]
    
    state$person_targets(targets)
    state$person_positions(positions)
    state$person_ids(targets$id)
    helpers$refresh_person_choices(targets, character())
    
    showNotification("Removed the selected person.", type = "message")
  })
  
  observeEvent(input$reset, {
    targets <- state$person_targets()
    if (!nrow(targets)) return()
    
    targets$a <- -1
    targets$b <- 2
    state$person_targets(targets)
    helpers$refresh_controls()
  })
  
  observeEvent(input$switch_branch, {
    selected <- state$selected_people()
    if (!length(selected)) {
      showNotification("Select a person first.", type = "message")
      return()
    }
    
    positions <- state$person_positions()
    switched <- 0L
    
    for (row in which(positions$id %in% selected)) {
      roots <- real_roots(positions$a[row], positions$b[row])
      stable_roots <- roots[3 * roots^2 - positions$b[row] > 1e-7]
      if (length(stable_roots) < 2) next
      
      furthest_root <- which.max(abs(stable_roots - positions$x[row]))
      positions$x[row] <- stable_roots[furthest_root]
      switched <- switched + 1L
    }
    
    if (switched > 0) state$person_positions(positions)
    
    showNotification(
      if (switched > 0) "Switched the selected person's branch."
      else "No second stable branch at this location, so nothing changed.",
      type = "message"
    )
  })
}


# 6. Calculation helper for movement
# No reactive state is needed here: the result depends only on the arguments
# and the existing equilibrium solver / max_control_step setting.
advance_person <- function(a, b, x, target_a, target_b, travel, distance) {
  start <- c(a, b)
  target <- c(target_a, target_b)
  
  endpoint <- if (travel >= distance) {
    target
  } else {
    start + (travel / distance) * (target - start)
  }
  
  number_of_steps <- max(1L, as.integer(ceiling(travel / max_control_step)))
  
  # Intermediate equilibria are internal; only the endpoint is displayed.
  for (step in seq_len(number_of_steps)) {
    controls <- start + (step / number_of_steps) * (endpoint - start)
    x <- choose_equilibrium(controls[1], controls[2], x)
  }
  
  list(a = endpoint[1], b = endpoint[2], x = x)
}


# 7. Movement timer
setup_movement <- function(input, session, state) {
  # This ordinary variable belongs to this setup call / browser session.
  # The observer's closure keeps it available after setup_movement returns.
  previous_update_time <- Sys.time()
  
  observe({
    invalidateLater(frame_ms, session)
    
    now <- Sys.time()
    elapsed_seconds <- as.numeric(difftime(
      now, previous_update_time, units = "secs"
    ))
    previous_update_time <<- now
    
    # Retain the original safeguard against very large delayed steps.
    elapsed_seconds <- min(elapsed_seconds, 0.15)
    speed <- isolate(input$speed)
    
    if (is.null(speed) || !is.finite(speed) || speed <= 0 ||
        elapsed_seconds <= 0) return()
    
    targets <- isolate(state$person_targets())
    positions <- isolate(state$person_positions())
    if (!nrow(positions)) return()
    
    # Both tables share row order. Only moving people need root solving.
    difference_a <- targets$a - positions$a
    difference_b <- targets$b - positions$b
    distances <- sqrt(difference_a^2 + difference_b^2)
    moving_rows <- which(distances >= 1e-10)
    if (!length(moving_rows)) return()
    
    for (row in moving_rows) {
      travel <- min(distances[row], speed * elapsed_seconds)
      
      next_position <- advance_person(
        a = positions$a[row], b = positions$b[row], x = positions$x[row],
        target_a = targets$a[row], target_b = targets$b[row],
        travel = travel, distance = distances[row]
      )
      
      positions$a[row] <- next_position$a
      positions$b[row] <- next_position$b
      positions$x[row] <- next_position$x
    }
    
    state$person_positions(positions)
  })
}


# 8. Position readout
setup_readout <- function(output, state, helpers) {
  output$coordinates <- renderText({
    selected <- state$selected_people()
    if (!length(selected)) return("No people selected.")
    
    targets <- state$person_targets()
    positions <- state$person_positions()
    labels <- helpers$axis_labels()
    
    blocks <- lapply(selected, function(id) {
      target_row <- match(id, targets$id)
      position_row <- match(id, positions$id)
      if (is.na(target_row) || is.na(position_row)) return(NULL)
      
      paste0(
        targets$name[target_row], "\n",
        labels$a, ": ", sprintf("%.3f", positions$a[position_row]), "\n",
        labels$b, ": ", sprintf("%.3f", positions$b[position_row]), "\n",
        labels$response, ": ", sprintf("%.3f", positions$x[position_row])
      )
    })
    
    paste(unlist(blocks), collapse = "\n\n")
  })
}


# 9. Plot: initial scene and in-place updates
setup_plot <- function(output, state, helpers) {
  # No reactive reads here: construct the initial scene once.
  output$cusp <- renderPlotly({
    hover <- hover_coordinates(default_labels)
    
    plot <- plot_ly() |>
      add_surface(
        x = A, y = B, z = X_stable, name = "Stable equilibria",
        opacity = 0.62, showscale = FALSE,
        colorscale = list(c(0, "#93c5fd"), c(1, "#93c5fd")),
        hovertemplate = paste0(hover, "<extra>Stable sheet</extra>")
      ) |>
      add_surface(
        x = A, y = B, z = X_unstable, name = "Unstable equilibria",
        opacity = 0.28, showscale = FALSE,
        colorscale = list(c(0, "#fdba74"), c(1, "#fdba74")),
        hovertemplate = paste0(hover, "<extra>Unstable sheet</extra>")
      ) |>
      add_trace(
        x = fold_a, y = fold_b, z = fold_x,
        type = "scatter3d", mode = "lines", name = "Folds",
        line = list(color = "#334155", width = 5), hoverinfo = "skip"
      ) |>
      layout(
        uirevision = "keep-camera", showlegend = FALSE,
        scene = list(
          uirevision = "keep-camera",
          xaxis = list(
            title = list(text = default_labels$a),
            range = c(normal_limits[1], normal_limits[2])
          ),
          yaxis = list(
            title = list(text = default_labels$b),
            range = c(splitting_limits[1], splitting_limits[2])
          ),
          zaxis = list(
            title = list(text = default_labels$response), range = c(-2.6, 3)
          ),
          aspectmode = "manual", aspectratio = list(x = 1.25, y = 1, z = 1),
          camera = list(eye = list(x = 1.6, y = 1.6, z = 1.1))
        ),
        margin = list(l = 0, r = 0, b = 0, t = 20)
      ) |>
      config(displaylogo = FALSE)
    
    plot <- plotly_build(plot)
    plot$x$data <- c(
      plot$x$data,
      person_traces(state$initial_targets, state$initial_positions, TRUE)
    )
    plot
  })
  
  # Position changes update coordinates, not the entire plot definition.
  # Selection alone does not trigger this observer.
  observeEvent(state$person_positions(), {
    positions <- state$person_positions()
    if (!nrow(positions)) return()
    
    geometries <- lapply(seq_len(nrow(positions)), function(row) {
      figure_geometry(positions$a[row], positions$b[row], positions$x[row])
    })
    
    a_values <- list()
    b_values <- list()
    x_values <- list()
    
    for (row in seq_len(nrow(positions))) {
      geometry <- geometries[[row]]
      a <- positions$a[row]
      b <- positions$b[row]
      x <- positions$x[row]
      
      # Each person contributes body, head, and foot-marker coordinates.
      # Explicit inner lists preserve single-point coordinate arrays.
      a_values <- c(a_values, list(geometry$a, list(a), list(a)))
      b_values <- c(b_values, list(geometry$b, list(b), list(b)))
      x_values <- c(x_values, list(geometry$x, list(geometry$head_x), list(x)))
    }
    
    helpers$update_plot(
      "restyle", list(x = a_values, y = b_values, z = x_values),
      helpers$all_person_trace_indices(seq_len(nrow(positions)))
    )
  }, ignoreInit = TRUE)
  
  # Selection changes only body thickness and head size.
  observeEvent(list(state$selected_people(), state$person_ids()), {
    ids <- state$person_ids()
    if (!length(ids)) return()
    
    selected <- ids %in% state$selected_people()
    bodies <- helpers$body_indices(seq_along(ids))
    
    helpers$update_plot(
      "restyle", list("line.width" = as.list(ifelse(selected, 8, 5))), bodies
    )
    helpers$update_plot(
      "restyle", list("marker.size" = as.list(ifelse(selected, 7, 5))),
      bodies + 1L
    )
  }, ignoreInit = TRUE)
  
  # Axis names update the titles and hover templates in place.
  observeEvent(helpers$axis_labels(), {
    labels <- helpers$axis_labels()
    
    helpers$update_plot("relayout", list(
      "scene.xaxis.title.text" = labels$a,
      "scene.yaxis.title.text" = labels$b,
      "scene.zaxis.title.text" = labels$response
    ))
    
    hover <- hover_coordinates(labels)
    helpers$update_plot("restyle", list(hovertemplate = list(
      paste0(hover, "<extra>Stable sheet</extra>"),
      paste0(hover, "<extra>Unstable sheet</extra>")
    )), c(0L, 1L))
    
    targets <- isolate(state$person_targets())
    if (!nrow(targets)) return()
    
    templates <- lapply(targets$name, anchor_hover, labels = labels)
    anchors <- helpers$body_indices(seq_len(nrow(targets))) + 2L
    helpers$update_plot("restyle", list(hovertemplate = templates), anchors)
  }, ignoreInit = TRUE)
}


# 10. Main server: connect the components
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
}
# ----------end----------

shinyApp(ui, server)