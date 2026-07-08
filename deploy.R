# deploy.R — publish the Phoenix Shiny app to shinyapps.io -------------------
#
# ONE-TIME SETUP
#   1. Create a free account at https://www.shinyapps.io
#   2. In the dashboard: Account > Tokens > (Show) > Copy to clipboard.
#      That copies a setAccountInfo(name=, token=, secret=) call already
#      filled in with your credentials.
#   3. Paste your three values into setAccountInfo() below.
#
#      >>> SECURITY: the token/secret are like a password. If you paste real
#      >>> values here, do NOT commit that change. (Better: run the copied
#      >>> setAccountInfo() line once in the R console — it's saved to your
#      >>> machine — and leave the placeholders here untouched.)
#
# DEPLOY / REDEPLOY
#   Run this whole script from the app folder. Redeploys reuse the same URL.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
library(rsconnect)

# --- your shinyapps.io credentials (from Account > Tokens) -------------------
setAccountInfo(
  name   = "YOUR_ACCOUNT_NAME",   # e.g. "andrewcwees" -> andrewcwees.shinyapps.io
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# --- deploy ------------------------------------------------------------------
deployApp(
  appDir      = ".",
  appName     = "phxmob",   # URL: https://YOUR_ACCOUNT_NAME.shinyapps.io/phxmob
  appTitle    = "Phoenix Multi-Modal Amenity Access",
  appFiles    = c("app.R", "phxmob_data.rds"),  # lean runtime-only bundle
  forceUpdate = TRUE
)
