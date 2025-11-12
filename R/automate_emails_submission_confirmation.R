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
df <- read_csv("data/call_for_collaboration/cleaned.csv") 

n_submissions <- n_distinct(df$code_name)
n_teams <- n_distinct(df$responseid)

# --- Example contact data ---
# df <- tibble(
#   name_first = c("Alice", "Alice", "Bob"),
#   email = c(rep("janlukas.pfaender@gmail.com", 3)),
#   code_name = c("sleepy walrus", "spicy otter", "lazy panda"),
#   intervention_name = c("some title", "another title", "a third title that's a bit longer")
# )

df <- df |>
  # add codenames with titles
  mutate(code_name = paste0(code_name, " (your original title: ", intervention_name, ")")
  ) |> 
  group_by(name_first, email) |>
  summarise(code_name = str_c(code_name, collapse = "; "), .groups = "drop") 

# clean a trouble maker email adress
df <- df |> 
  mutate(email = ifelse(name_first == "Inkuk", "inkuk.kim@outlook.com", email))


# --- Email message template ---
create_email_message <- function(name_first, code_name, n_submissions, n_teams) {
  glue("
Dear {name_first},

Thank you so much for submitting your intervention(s) to our collaborative megastudy on strengthening trust in climate scientists.

We have received a total of {n_submissions} intervention submissions from {n_teams} different research teams. 🎉

The advisory board will now start reviewing all interventions. The review process is anonymized—only the research lead, Jan, knows your identity and will not be able to vote.

For full transparency, once the advisory board has made their selection on which interventions to test, we will make the rankings public.

Your intervention code name(s): {code_name}

Codenames were randomly generated with the [codename package](https://svmiller.com/codename/). You will need the code name(s) later to identify your intervention in the anonymized review results.

🔗 **More information**

For more information about the project and the team, visit [our website](https://janpfander.github.io/trust_climate_scientists/).

💌 Questions? Email us anytime at trustclimsci.megastudy@gmail.com 

Again, thanks so much for your contribution!

Best regards,  
Jan and the Megastudy Team


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



# --- OLD alternative ---

# first try with the blastula package, which initially worked, but then didn't. Probably related to the server
# library(blastula)
# 
# # set credentials from Rprofile
# my_email_creds <- creds_envvar(
#   user = Sys.getenv("my_gmail"),
#   pass_envvar = "my_gmail_app_passcode", 
#   provider = "gmail"
# )
# 
# 
# # read relevant information from submission data
# submissions <- read_csv("data/call_for_collaboration/cleaned.csv")
# 
# n_submissions <-  n_distinct(submissions$code_name)
# n_teams <- n_distinct(submissions$responseid)
# 
# 
# # read data with contact details
# df <- tibble(
#   name = c("Alice", "Alice",  "Bob"),
#   email = c(rep("janlukas.pfaender@gmail.com", 3)),
#   code_names = c("sleepy walrus", "spicy otter", "lazy panda")
# )
# 
# df |> 
#   group_by(name, email) |> 
#   summarise(code_name = str_c(code_names, collapse = "; "))
# 
# 
# # write message with custom elements from data frame
# create_email_message <- function(name, code_names, n_submissions, n_teams) {
#   glue("
#     Dear {name},
# 
#     Thank you so much for submitting your intervention(s) to our collaborative megastudy on strenghtening trust in climate scientists. 
#     
#     We have received a total of {n_submissions} intervention submissions from {n_teams} different research teams. 
#     
#     The advisory board will now start reviewing all interventions. The review process is anonymized---only the research lead, Jan, your identiy and will not be able to vote.
#     
#     For full transparency, we will make the advisory board members' individual reviews public once available. 
# 
#     Your intervention code name(s): {code_names}
# 
#     This codename was randomly generated with the [`codename` package](https://svmiller.com/codename/). You will need the code name of your intervention(s) later, when we publish the outcome of the review process, to identify your intervention in the anonymized spreadsheet. 
# 
#     Best regards,
#     The Megastudy Team
#   ")
# }
# 
# # loop message over data frame
# for (i in seq_len(nrow(df))) {
#   
#   # Get recipient info
#   recipient_name <- df$name[i]
#   recipient_email <- df$email[i]
#   recipient_codes <- df$code_names[i]
#   
#   # Create personalized message
#   message_text <- create_email_message(recipient_name, recipient_codes, n_submissions, n_teams)
#   
#   # Compose the email
#   email <- compose_email(
#     body = md(message_text)
#   )
#   
#   # Send the email (replace smtp settings with yours)
#   smtp_send(
#     email,
#     to = recipient_email,
#     from = "my_gmail",
#     subject = "Your intervention submission",
#     credentials = my_email_creds  
#   )
#   
#   cat("Email sent to:", recipient_name, "\n")
# }



