# Script to automatically send an email confirmation to everyone who submitted

# --- Load packages ---
library(tidyverse)
library(glue)
library(emayili)
library(blastula)

# --- Gmail SMTP setup ---
smtp <- server(
  host = "smtp.gmail.com",
  port = 587,          # use 465 for SSL; also possible to try with 587
  username = Sys.getenv("my_gmail"),
  password = Sys.getenv("my_gmail_app_passcode"),
  reuse = FALSE
)

# --- Read submission data ---
df <- read_csv("data/call_for_collaboration/first_round.csv") 

# --- Email message template ---
create_email_message <- function(name_first, code_name, n_submissions, n_teams) {
  glue("
Dear {name_first},

I'm sorry, but I have just re-checked the submission data and it appears that we have received many(!) additional last-minute/late submssions. Those were not yet taken into account in our previous email.  

Therefore, **please ignore our previous e-mail**. We will soon send you an updated version, with updated submission numbers. 

Note that incorporating the new data also means that you will receive a **new codename(s)**. I hope you didn't get too attached to the previous one(s) yet :)

I'm sorry for the confusion! Thank you for your patience.

All the best,  
Jan


  ")
}

# --- Send personalized emails ---
for (i in seq_len(nrow(df))) {
  
  recipient_name <- df$name_first[i]
  recipient_email <- df$email[i]
  recipient_codes <- df$code_name[i]
  
  message_text <- create_email_message(recipient_name, recipient_codes, n_submissions, n_teams)
  
  email <- envelope() |>
    from(Sys.getenv("my_gmail")) |>
    to(recipient_email) |>
    subject("Your intervention submission") |>
    # use render for markdown content
    render(message_text)
  
  # Send email
  smtp(email, verbose = TRUE)
  
  cat("Email sent to:", recipient_name, "-", recipient_email, "\n")
  
  Sys.sleep(1)  # pause 1 second before next iteration
}





