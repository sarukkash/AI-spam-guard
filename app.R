# ============================================================
# AI SPAM GUARD
# Automatic Incoming Message Detection
# ============================================================

library(shiny)
library(shinydashboard)
library(quanteda)
library(glmnet)
library(Matrix)

# ------------------------------------------------------------
# LOAD SAVED AI MODEL
# ------------------------------------------------------------

load("AI_Spam_Guard.RData")

# ------------------------------------------------------------
# TEXT CLEANING
# ------------------------------------------------------------

clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[^a-z ]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

# ------------------------------------------------------------
# SPAM PREDICTION FUNCTION
# ------------------------------------------------------------

predict_spam <- function(message) {
  
  message_clean <- clean_text(message)
  
  # ---------------- WORD N-GRAMS ----------------
  
  word_tokens <- tokens(
    message_clean,
    what = "word"
  )
  
  word_ngrams <- tokens_ngrams(
    word_tokens,
    n = 1:2
  )
  
  word_dfm <- dfm(word_ngrams)
  
  word_dfm <- dfm_match(
    word_dfm,
    features = featnames(train_dfm_ngram)
  )
  
  # ---------------- CHARACTER N-GRAMS ----------------
  
  char_tokens <- tokens(
    message_clean,
    what = "character",
    remove_separators = TRUE
  )
  
  char_ngrams <- tokens_ngrams(
    char_tokens,
    n = 3:5
  )
  
  char_dfm <- dfm(char_ngrams)
  
  char_dfm <- dfm_match(
    char_dfm,
    features = featnames(train_dfm_char)
  )
  
  # ---------------- COMBINE FEATURES ----------------
  
  combined_dfm <- cbind(
    word_dfm,
    char_dfm
  )
  
  new_data <- as(combined_dfm, "dgCMatrix")
  
  # ---------------- PREDICT ----------------
  
  probability <- as.numeric(
    predict(
      spam_model_combined,
      newx = new_data,
      type = "response"
    )
  )
  
  return(probability)
}

# ------------------------------------------------------------
# SPAM REASONS
# ------------------------------------------------------------

get_spam_reasons <- function(message) {
  
  message <- tolower(message)
  
  reasons <- c()
  
  if (grepl(
    "win|winner|won|prize|reward|lottery|cash",
    message
  )) {
    reasons <- c(
      reasons,
      "Prize / reward related words"
    )
  }
  
  if (grepl(
    "free|offer|discount|claim|bonus",
    message
  )) {
    reasons <- c(
      reasons,
      "Promotional words"
    )
  }
  
  if (grepl(
    "click|link|http|www|url",
    message
  )) {
    reasons <- c(
      reasons,
      "Suspicious link / click request"
    )
  }
  
  if (grepl(
    "urgent|immediately|limited|act now",
    message
  )) {
    reasons <- c(
      reasons,
      "Urgency-related words"
    )
  }
  
  if (length(reasons) == 0) {
    reasons <- "No obvious suspicious keywords detected"
  }
  
  return(reasons)
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- dashboardPage(
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  dashboardHeader(
    title = "AI Spam Guard"
  ),
  
  # ----------------------------------------------------------
  # SIDEBAR
  # ----------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Live Inbox",
        tabName = "live_inbox",
        icon = icon("inbox")
      ),
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Sender Rules",
        tabName = "sender_rules",
        icon = icon("user-shield")
      ),
      
      menuItem(
        "Message History",
        tabName = "message_history",
        icon = icon("history")
      )
      
    )
  ),
  
  # ----------------------------------------------------------
  # BODY
  # ----------------------------------------------------------
  
  dashboardBody(
    
    # --------------------------------------------------------
    # LIVE INBOX
    # --------------------------------------------------------
    
    tabItems(
      
      tabItem(
        
        tabName = "live_inbox",
        
        fluidRow(
          
          box(
            width = 12,
            title = "📥 Live Inbox",
            status = "primary",
            solidHeader = TRUE,
            
            h4(
              "Automatically checking incoming messages..."
            ),
            
            p(
              "New messages are read from incoming_messages.csv every 5 seconds."
            ),
            
            tags$hr(),
            
            tableOutput("live_messages")
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Latest Message",
            status = "warning",
            solidHeader = TRUE,
            
            h4(
              textOutput("latest_sender")
            ),
            
            tags$hr(),
            
            p(
              textOutput("latest_message")
            )
            
          ),
          
          box(
            width = 6,
            title = "AI Result",
            status = "success",
            solidHeader = TRUE,
            
            h3(
              textOutput("latest_result")
            ),
            
            p(
              strong("Spam Probability:")
            ),
            
            h4(
              textOutput("latest_probability")
            ),
            
            p(
              strong("Reason:")
            ),
            
            textOutput("latest_reasons")
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "User Action",
            status = "danger",
            solidHeader = TRUE,
            
            p(
              "For a new spam message, choose what to do with the sender:"
            ),
            
            actionButton(
              "block_sender",
              "BLOCK",
              class = "btn-danger"
            ),
            
            actionButton(
              "mute_sender",
              "MUTE",
              class = "btn-warning"
            ),
            
            actionButton(
              "allow_sender",
              "ALLOW",
              class = "btn-success"
            )
            
          )
          
        )
        
      ),
      
      # ======================================================
      # DASHBOARD
      # ======================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput(
            "total_messages",
            width = 4
          ),
          
          valueBoxOutput(
            "total_spam",
            width = 4
          ),
          
          valueBoxOutput(
            "total_safe",
            width = 4
          )
          
        ),
        
        fluidRow(
          
          valueBoxOutput(
            "total_blocked",
            width = 4
          ),
          
          valueBoxOutput(
            "total_muted",
            width = 4
          ),
          
          valueBoxOutput(
            "total_allowed",
            width = 4
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Spam vs Safe",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "spam_safe_chart",
              height = "300px"
            )
          ),
          
          box(
            width = 6,
            title = "User Actions",
            status = "success",
            solidHeader = TRUE,
            
            plotOutput(
              "action_chart",
              height = "300px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Average Spam Probability",
            status = "warning",
            solidHeader = TRUE,
            
            h2(
              textOutput("average_probability")
            )
            
          )
          
        )
        
      ),
      
      # ======================================================
      # SENDER RULES
      # ======================================================
      
      tabItem(
        
        tabName = "sender_rules",
        
        fluidRow(
          
          box(
            width = 12,
            title = "📋 Saved Sender Rules",
            status = "primary",
            solidHeader = TRUE,
            
            p(
              "The AI remembers your decision for each sender."
            ),
            
            tableOutput("rules_table")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # MESSAGE HISTORY
      # ======================================================
      
      tabItem(
        
        tabName = "message_history",
        
        fluidRow(
          
          box(
            width = 12,
            title = "📜 Message History",
            status = "primary",
            solidHeader = TRUE,
            
            tableOutput("history_table")
            
          )
          
        )
        
      )
      
    )
    
  )
  
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # LOAD SENDER RULES
  # ----------------------------------------------------------
  
  if (file.exists("sender_rules.csv")) {
    
    rules <- read.csv(
      "sender_rules.csv",
      stringsAsFactors = FALSE,
      colClasses = "character"
    )
    
  } else {
    
    rules <- data.frame(
      Sender = character(),
      Action = character(),
      stringsAsFactors = FALSE
    )
    
  }
  
  sender_rules <- reactiveVal(rules)
  
  # ----------------------------------------------------------
  # LOAD MESSAGE HISTORY
  # ----------------------------------------------------------
  
  if (file.exists("message_history.csv")) {
    
    history <- read.csv(
      "message_history.csv",
      stringsAsFactors = FALSE,
      colClasses = c(
        "character",
        "character",
        "character",
        "numeric",
        "character",
        "character"
      )
    )
    
  } else {
    
    history <- data.frame(
      ID = character(),
      Sender = character(),
      Message = character(),
      Probability = numeric(),
      Classification = character(),
      Action = character(),
      stringsAsFactors = FALSE
    )
    
  }
  
  message_history <- reactiveVal(history)
  
  # ----------------------------------------------------------
  # LATEST MESSAGE INFORMATION
  # ----------------------------------------------------------
  
  latest_message <- reactiveVal(
    data.frame(
      ID = "",
      Sender = "",
      Message = "",
      Probability = NA,
      Classification = "",
      Action = "",
      stringsAsFactors = FALSE
    )
  )
  
  # ----------------------------------------------------------
  # PROCESSED MESSAGE IDS
  # ----------------------------------------------------------
  
  processed_messages <- reactiveVal(
    unique(history$ID)
  )
  
  # ----------------------------------------------------------
  # SAVE RULES
  # ----------------------------------------------------------
  
  save_rules <- function() {
    
    write.csv(
      sender_rules(),
      "sender_rules.csv",
      row.names = FALSE
    )
    
  }
  
  # ----------------------------------------------------------
  # SAVE HISTORY
  # ----------------------------------------------------------
  
  save_history <- function() {
    
    write.csv(
      message_history(),
      "message_history.csv",
      row.names = FALSE
    )
    
  }
  
  # ----------------------------------------------------------
  # AUTOMATIC MESSAGE PROCESSING
  # ----------------------------------------------------------
  
  process_incoming_messages <- function() {
    
    if (!file.exists("incoming_messages.csv")) {
      return()
    }
    
    incoming <- tryCatch(
      
      read.csv(
        "incoming_messages.csv",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      
      error = function(e) {
        return(NULL)
      }
      
    )
    
    if (is.null(incoming)) {
      return()
    }
    
    # --------------------------------------------------------
    # CHECK REQUIRED COLUMNS
    # --------------------------------------------------------
    
    if (!all(
      c("Sender", "Message") %in% names(incoming)
    )) {
      
      return()
      
    }
    
    # --------------------------------------------------------
    # CREATE ID IF NOT PRESENT
    # --------------------------------------------------------
    
    if (!"id" %in% names(incoming) &&
        !"ID" %in% names(incoming)) {
      
      incoming$ID <- paste0(
        incoming$Sender,
        "_",
        seq_len(nrow(incoming))
      )
      
    } else if ("id" %in% names(incoming)) {
      
      incoming$ID <- as.character(incoming$id)
      
    } else {
      
      incoming$ID <- as.character(incoming$ID)
      
    }
    
    # --------------------------------------------------------
    # PROCESS EACH MESSAGE
    # --------------------------------------------------------
    
    for (i in seq_len(nrow(incoming))) {
      
      message_id <- as.character(incoming$ID[i])
      
      if (message_id %in% processed_messages()) {
        next
      }
      
      sender <- trimws(
        as.character(incoming$Sender[i])
      )
      
      message <- as.character(
        incoming$Message[i]
      )
      
      if (
        sender == "" ||
        message == ""
      ) {
        next
      }
      
      # ------------------------------------------------------
      # CHECK SAVED SENDER RULE
      # ------------------------------------------------------
      
      rules <- sender_rules()
      
      saved_rule <- rules[
        rules$Sender == sender,
        ,
        drop = FALSE
      ]
      
      if (nrow(saved_rule) > 0) {
        
        action <- saved_rule$Action[1]
        
        # Saved rule controls ACTION.
        # It does NOT change AI probability.
        
        classification <- "RULE APPLIED"
        probability <- NA
        reasons <- paste(
          "Saved sender rule:",
          action
        )
        
      } else {
        
        # ----------------------------------------------------
        # AI PREDICTION
        # ----------------------------------------------------
        
        probability <- predict_spam(message)
        
        if (probability >= 0.20) {
          
          classification <- "SPAM"
          
          action <- "PENDING"
          
          reasons <- paste(
            get_spam_reasons(message),
            collapse = "; "
          )
          
        } else {
          
          classification <- "SAFE"
          
          action <- "ALLOW"
          
          reasons <- "Message classified as safe."
          
        }
        
      }
      
      # ------------------------------------------------------
      # ADD TO HISTORY
      # ------------------------------------------------------
      
      new_row <- data.frame(
        
        ID = message_id,
        
        Sender = sender,
        
        Message = message,
        
        Probability = probability,
        
        Classification = classification,
        
        Action = action,
        
        stringsAsFactors = FALSE
        
      )
      
      old_history <- message_history()
      
      message_history(
        rbind(
          old_history,
          new_row
        )
      )
      
      processed_messages(
        c(
          processed_messages(),
          message_id
        )
      )
      
      # ------------------------------------------------------
      # UPDATE LATEST MESSAGE
      # ------------------------------------------------------
      
      latest_message(
        new_row
      )
      
      # ------------------------------------------------------
      # SAVE
      # ------------------------------------------------------
      
      save_history()
      
      # ------------------------------------------------------
      # SHOW ALERT
      # ------------------------------------------------------
      
      if (classification == "SPAM") {
        
        showNotification(
          paste(
            "🚨 SPAM detected from",
            sender
          ),
          type = "error",
          duration = 8
        )
        
      } else if (classification == "SAFE") {
        
        showNotification(
          paste(
            "✅ Safe message received from",
            sender
          ),
          type = "message",
          duration = 4
        )
        
      } else {
        
        showNotification(
          paste(
            "📌 Saved rule applied to",
            sender
          ),
          type = "warning",
          duration = 4
        )
        
      }
      
    }
    
  }
  
  # ----------------------------------------------------------
  # AUTOMATIC CHECK EVERY 5 SECONDS
  # ----------------------------------------------------------
  
  observe({
    
    invalidateLater(
      5000,
      session
    )
    
    process_incoming_messages()
    
  })
  
  # ==========================================================
  # LIVE INBOX
  # ==========================================================
  
  output$live_messages <- renderTable({
    
    history <- message_history()
    
    if (nrow(history) == 0) {
      
      return(
        data.frame(
          Message = "No messages received yet."
        )
      )
      
    }
    
    display <- tail(
      history,
      10
    )
    
    display <- display[
      ,
      c(
        "Sender",
        "Message",
        "Classification",
        "Action"
      ),
      drop = FALSE
    ]
    
    display
    
  })
  
  # ----------------------------------------------------------
  # LATEST SENDER
  # ----------------------------------------------------------
  
  output$latest_sender <- renderText({
    
    latest <- latest_message()
    
    if (latest$Sender == "") {
      return("Waiting for incoming message...")
    }
    
    paste(
      "Sender:",
      latest$Sender
    )
    
  })
  
  # ----------------------------------------------------------
  # LATEST MESSAGE
  # ----------------------------------------------------------
  
  output$latest_message <- renderText({
    
    latest <- latest_message()
    
    if (latest$Message == "") {
      return("No message received yet.")
    }
    
    latest$Message
    
  })
  
  # ----------------------------------------------------------
  # LATEST RESULT
  # ----------------------------------------------------------
  
  output$latest_result <- renderText({
    
    latest <- latest_message()
    
    if (latest$Classification == "") {
      return("Waiting...")
    }
    
    paste(
      latest$Classification,
      "| Action:",
      latest$Action
    )
    
  })
  
  # ----------------------------------------------------------
  # LATEST PROBABILITY
  # ----------------------------------------------------------
  
  output$latest_probability <- renderText({
    
    latest <- latest_message()
    
    if (
      is.na(latest$Probability) ||
      latest$Classification == "RULE APPLIED"
    ) {
      
      return(
        "Not calculated because a saved sender rule was applied."
      )
      
    }
    
    paste0(
      round(
        latest$Probability * 100,
        2
      ),
      "%"
    )
    
  })
  
  # ----------------------------------------------------------
  # LATEST REASONS
  # ----------------------------------------------------------
  
  output$latest_reasons <- renderText({
    
    latest <- latest_message()
    
    if (latest$Classification == "") {
      return("Waiting...")
    }
    
    if (latest$Classification == "RULE APPLIED") {
      
      return(
        paste(
          "Saved action:",
          latest$Action
        )
      )
      
    }
    
    paste(
      get_spam_reasons(latest$Message),
      collapse = "; "
    )
    
  })
  
  # ==========================================================
  # USER ACTIONS
  # ==========================================================
  
  apply_sender_action <- function(action_name) {
    
    latest <- latest_message()
    
    if (latest$Sender == "") {
      
      showNotification(
        "No sender available.",
        type = "warning"
      )
      
      return()
      
    }
    
    sender <- latest$Sender
    
    rules <- sender_rules()
    
    # Remove old rule for this sender
    
    rules <- rules[
      rules$Sender != sender,
      ,
      drop = FALSE
    ]
    
    # Add new rule
    
    rules <- rbind(
      
      rules,
      
      data.frame(
        Sender = sender,
        Action = action_name,
        stringsAsFactors = FALSE
      )
      
    )
    
    sender_rules(rules)
    
    save_rules()
    
    # --------------------------------------------------------
    # UPDATE LATEST HISTORY ACTION
    # --------------------------------------------------------
    
    history <- message_history()
    
    matching_rows <- which(
      history$ID == latest$ID
    )
    
    if (length(matching_rows) > 0) {
      
      history$Action[
        matching_rows[length(matching_rows)]
      ] <- action_name
      
      message_history(history)
      
      save_history()
      
    }
    
    # --------------------------------------------------------
    # UPDATE LATEST DISPLAY
    # --------------------------------------------------------
    
    latest$Action <- action_name
    
    latest_message(latest)
    
    showNotification(
      
      paste(
        "Sender",
        sender,
        "is now set to",
        action_name
      ),
      
      type = "message",
      duration = 5
      
    )
    
  }
  
  # ----------------------------------------------------------
  # BLOCK
  # ----------------------------------------------------------
  
  observeEvent(
    input$block_sender,
    {
      apply_sender_action("BLOCK")
    }
  )
  
  # ----------------------------------------------------------
  # MUTE
  # ----------------------------------------------------------
  
  observeEvent(
    input$mute_sender,
    {
      apply_sender_action("MUTE")
    }
  )
  
  # ----------------------------------------------------------
  # ALLOW
  # ----------------------------------------------------------
  
  observeEvent(
    input$allow_sender,
    {
      apply_sender_action("ALLOW")
    }
  )
  
  # ==========================================================
  # DASHBOARD
  # ==========================================================
  
  output$total_messages <- renderValueBox({
    
    history <- message_history()
    
    valueBox(
      nrow(history),
      "Total Messages",
      icon = icon("envelope"),
      color = "blue"
    )
    
  })
  
  # ----------------------------------------------------------
  
  output$total_spam <- renderValueBox({
    
    history <- message_history()
    
    count <- sum(
      history$Classification == "SPAM",
      na.rm = TRUE
    )
    
    valueBox(
      count,
      "Spam Messages",
      icon = icon("ban"),
      color = "red"
    )
    
  })
  
  # ----------------------------------------------------------
  
  output$total_safe <- renderValueBox({
    
    history <- message_history()
    
    count <- sum(
      history$Classification == "SAFE",
      na.rm = TRUE
    )
    
    valueBox(
      count,
      "Safe Messages",
      icon = icon("check"),
      color = "green"
    )
    
  })
  
  # ----------------------------------------------------------
  
  output$total_blocked <- renderValueBox({
    
    history <- message_history()
    
    count <- sum(
      history$Action == "BLOCK",
      na.rm = TRUE
    )
    
    valueBox(
      count,
      "Blocked",
      icon = icon("ban"),
      color = "red"
    )
    
  })
  
  # ----------------------------------------------------------
  
  output$total_muted <- renderValueBox({
    
    history <- message_history()
    
    count <- sum(
      history$Action == "MUTE",
      na.rm = TRUE
    )
    
    valueBox(
      count,
      "Muted",
      icon = icon("volume-mute"),
      color = "yellow"
    )
    
  })
  
  # ----------------------------------------------------------
  
  output$total_allowed <- renderValueBox({
    
    history <- message_history()
    
    count <- sum(
      history$Action == "ALLOW",
      na.rm = TRUE
    )
    
    valueBox(
      count,
      "Allowed",
      icon = icon("check-circle"),
      color = "green"
    )
    
  })
  
  # ==========================================================
  # SPAM VS SAFE CHART
  # ==========================================================
  
  output$spam_safe_chart <- renderPlot({
    
    history <- message_history()
    
    spam_count <- sum(
      history$Classification == "SPAM",
      na.rm = TRUE
    )
    
    safe_count <- sum(
      history$Classification == "SAFE",
      na.rm = TRUE
    )
    
    data <- c(
      spam_count,
      safe_count
    )
    
    names(data) <- c(
      "SPAM",
      "SAFE"
    )
    
    barplot(
      data,
      main = "Spam vs Safe Messages",
      ylab = "Number of Messages",
      col = c(
        "red",
        "green"
      )
    )
    
  })
  
  # ==========================================================
  # ACTION CHART
  # ==========================================================
  
  output$action_chart <- renderPlot({
    
    history <- message_history()
    
    blocked <- sum(
      history$Action == "BLOCK",
      na.rm = TRUE
    )
    
    muted <- sum(
      history$Action == "MUTE",
      na.rm = TRUE
    )
    
    allowed <- sum(
      history$Action == "ALLOW",
      na.rm = TRUE
    )
    
    data <- c(
      blocked,
      muted,
      allowed
    )
    
    names(data) <- c(
      "BLOCK",
      "MUTE",
      "ALLOW"
    )
    
    barplot(
      data,
      main = "User Actions",
      ylab = "Number of Messages",
      col = c(
        "red",
        "orange",
        "green"
      )
      
    )
    
  })
  
  # ==========================================================
  # AVERAGE PROBABILITY
  # ==========================================================
  
  output$average_probability <- renderText({
    
    history <- message_history()
    
    probabilities <- history$Probability[
      !is.na(history$Probability)
    ]
    
    if (length(probabilities) == 0) {
      return("0%")
    }
    
    paste0(
      round(
        mean(probabilities) * 100,
        2
      ),
      "%"
    )
    
  })
  
  # ==========================================================
  # SENDER RULES TABLE
  # ==========================================================
  
  output$rules_table <- renderTable({
    
    rules <- sender_rules()
    
    if (nrow(rules) == 0) {
      
      return(
        data.frame(
          Information = "No sender rules saved yet."
        )
      )
      
    }
    
    rules
    
  })
  
  # ==========================================================
  # MESSAGE HISTORY TABLE
  # ==========================================================
  
  output$history_table <- renderTable({
    
    history <- message_history()
    
    if (nrow(history) == 0) {
      
      return(
        data.frame(
          Information = "No message history yet."
        )
      )
      
    }
    
    history
    
  })
  
}

# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)