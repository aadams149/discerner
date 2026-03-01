library(shiny)
library(jsonlite)

# --- Helpers --------------------------------------------------------------

parse_items <- function(text) {
  # Split on newlines and commas
  items <- unlist(strsplit(text, "[,\n]+"))
  items <- trimws(items)
  items <- items[nzchar(items)]
  unique(items)
}

estimate_max_comparisons <- function(n) {
  # Worst-case comparisons for binary insertion sort
  # sum(ceiling(log2(k))) for k = 1..(n-1)
  if (n <= 1L) return(0L)
  sum(ceiling(log2(seq_len(n - 1L))))
}

get_save_state <- function(state) {
  list(
    items = state$items,
    sorted = state$sorted,
    remaining = state$remaining,
    current_item = state$current_item,
    low = state$low,
    high = state$high,
    mid = state$mid,
    battle_num = state$battle_num,
    max_battles = state$max_battles,
    comparisons = state$comparisons
  )
}

restore_state <- function(state, saved) {
  state$items <- saved$items
  state$sorted <- saved$sorted
  state$remaining <- saved$remaining
  state$current_item <- saved$current_item
  state$low <- as.integer(saved$low)
  state$high <- as.integer(saved$high)
  state$mid <- as.integer(saved$mid)
  state$battle_num <- as.integer(saved$battle_num)
  state$max_battles <- as.integer(saved$max_battles)
  state$comparisons <- saved$comparisons
  state$phase <- "battle"
}

# --- UI -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .battle-btn {
      font-size: 24px;
      padding: 40px 20px;
      width: 100%;
      white-space: normal;
      word-wrap: break-word;
    }
    .result-table {
      font-size: 18px;
    }
    .progress-text {
      font-size: 16px;
      color: #555;
      margin-bottom: 15px;
    }
    .phase-panel {
      max-width: 700px;
      margin: 0 auto;
      padding-top: 30px;
    }
    .save-section {
      margin-top: 20px;
      padding-top: 15px;
      border-top: 1px solid #eee;
    }
  "))),

  div(class = "phase-panel",
    titlePanel("discerner"),
    p("Rank items by preference using pairwise comparisons."),

    # --- Input phase ---
    conditionalPanel(
      condition = "output.phase == 'input'",
      textAreaInput(
        "items_text",
        "Enter items to rank (one per line, or comma-separated):",
        rows = 8,
        placeholder = "pizza\ntacos\nsushi\nburgers"
      ),
      actionButton("start_btn", "Start Sorting", class = "btn-primary btn-lg"),
      uiOutput("input_error"),
      div(class = "save-section",
        h4("Resume a previous session"),
        fileInput("load_file", NULL,
                  accept = c(".rds", ".json"),
                  buttonLabel = "Load session file",
                  placeholder = ".rds or .json")
      )
    ),

    # --- Battle phase ---
    conditionalPanel(
      condition = "output.phase == 'battle'",
      h3("Which do you prefer?"),
      uiOutput("battle_progress"),
      fluidRow(
        column(6, actionButton("pick_a", "", class = "btn-default battle-btn")),
        column(6, actionButton("pick_b", "", class = "btn-default battle-btn"))
      ),
      div(class = "save-section",
        fluidRow(
          column(4, radioButtons("save_format", "Format:",
                                 choices = c("JSON" = "json", "RDS" = "rds"),
                                 inline = TRUE)),
          column(4, downloadButton("save_progress", "Save Progress"))
        )
      )
    ),

    # --- Results phase ---
    conditionalPanel(
      condition = "output.phase == 'results'",
      h3("Final Rankings"),
      tableOutput("results_table"),
      br(),
      fluidRow(
        column(4, downloadButton("download_csv", "Download CSV")),
        column(4, actionButton("restart_btn", "Start Over", class = "btn-default"))
      )
    )
  )
)

# --- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive state
  state <- reactiveValues(
    phase = "input",
    items = character(0),
    sorted = character(0),
    remaining = character(0),
    current_item = NULL,
    low = 1L,
    high = 0L,
    mid = 1L,
    battle_num = 0L,
    max_battles = 0L,
    comparisons = list()
  )

  # Expose phase for conditionalPanel
  output$phase <- reactive(state$phase)
  outputOptions(output, "phase", suspendWhenHidden = FALSE)

  # --- Input phase ---------------------------------------------------------

  observeEvent(input$start_btn, {
    items <- parse_items(input$items_text)

    if (length(items) < 2L) {
      output$input_error <- renderUI(
        tags$p(style = "color: red; margin-top: 10px;",
               "Please enter at least 2 unique items.")
      )
      return()
    }

    output$input_error <- renderUI(NULL)

    # Initialize sorting state
    state$items <- items
    state$sorted <- items[1L]
    state$remaining <- items[-1L]
    state$battle_num <- 0L
    state$max_battles <- estimate_max_comparisons(length(items))
    state$comparisons <- list()

    # Start first insertion
    start_next_insertion()
  })

  # --- Load session --------------------------------------------------------

  observeEvent(input$load_file, {
    req(input$load_file)
    file_path <- input$load_file$datapath
    file_name <- input$load_file$name
    ext <- tolower(tools::file_ext(file_name))

    saved <- tryCatch({
      if (ext == "rds") {
        readRDS(file_path)
      } else if (ext == "json") {
        fromJSON(file_path, simplifyVector = TRUE)
      } else {
        stop("Unsupported file type. Please use .rds or .json.")
      }
    }, error = function(e) {
      output$input_error <- renderUI(
        tags$p(style = "color: red; margin-top: 10px;", e$message)
      )
      return(NULL)
    })

    if (is.null(saved)) return()

    # Validate the saved state has required fields
    required <- c("items", "sorted", "remaining", "current_item",
                   "low", "high", "mid", "battle_num", "max_battles")
    if (!all(required %in% names(saved))) {
      output$input_error <- renderUI(
        tags$p(style = "color: red; margin-top: 10px;",
               "Invalid session file: missing required fields.")
      )
      return()
    }

    output$input_error <- renderUI(NULL)
    restore_state(state, saved)

    # Update battle buttons for current comparison
    updateActionButton(session, "pick_a", label = state$current_item)
    updateActionButton(session, "pick_b", label = state$sorted[state$mid])
  })

  # --- Battle state machine ------------------------------------------------

  start_next_insertion <- function() {
    if (length(state$remaining) == 0L) {
      state$phase <- "results"
      return()
    }

    state$current_item <- state$remaining[1L]
    state$remaining <- state$remaining[-1L]
    state$low <- 1L
    state$high <- length(state$sorted)

    advance_battle()
  }

  advance_battle <- function() {
    if (state$low > state$high) {
      # Insert at position `low`
      pos <- state$low
      if (pos == 1L) {
        state$sorted <- c(state$current_item, state$sorted)
      } else if (pos > length(state$sorted)) {
        state$sorted <- c(state$sorted, state$current_item)
      } else {
        state$sorted <- append(state$sorted, state$current_item, after = pos - 1L)
      }
      start_next_insertion()
      return()
    }

    state$mid <- (state$low + state$high) %/% 2L
    state$battle_num <- state$battle_num + 1L
    state$phase <- "battle"

    # Update button labels
    updateActionButton(session, "pick_a", label = state$current_item)
    updateActionButton(session, "pick_b", label = state$sorted[state$mid])
  }

  # --- Battle phase --------------------------------------------------------

  output$battle_progress <- renderUI({
    div(class = "progress-text",
      sprintf("Battle %d of ~%d", state$battle_num, state$max_battles)
    )
  })

  observeEvent(input$pick_a, {
    # User prefers current_item over sorted[mid]
    state$comparisons[[length(state$comparisons) + 1L]] <- c(
      winner = state$current_item,
      loser = state$sorted[state$mid]
    )
    state$high <- state$mid - 1L
    advance_battle()
  })

  observeEvent(input$pick_b, {
    # User prefers sorted[mid] over current_item
    state$comparisons[[length(state$comparisons) + 1L]] <- c(
      winner = state$sorted[state$mid],
      loser = state$current_item
    )
    state$low <- state$mid + 1L
    advance_battle()
  })

  # --- Save progress -------------------------------------------------------

  output$save_progress <- downloadHandler(
    filename = function() {
      fmt <- input$save_format
      paste0("discerner_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
    },
    content = function(file) {
      saved <- get_save_state(state)
      if (input$save_format == "rds") {
        saveRDS(saved, file)
      } else {
        writeLines(toJSON(saved, auto_unbox = TRUE, pretty = TRUE), file)
      }
    }
  )

  # --- Results phase -------------------------------------------------------

  output$results_table <- renderTable({
    req(state$phase == "results")
    data.frame(
      Rank = seq_along(state$sorted),
      Item = state$sorted,
      stringsAsFactors = FALSE
    )
  }, class = "result-table", striped = TRUE, hover = TRUE)

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("discerner_ranking_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- data.frame(
        rank = seq_along(state$sorted),
        item = state$sorted,
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  observeEvent(input$restart_btn, {
    state$phase <- "input"
    state$sorted <- character(0)
    state$remaining <- character(0)
    state$current_item <- NULL
    state$comparisons <- list()
    state$battle_num <- 0L
  })
}

shinyApp(ui, server)
