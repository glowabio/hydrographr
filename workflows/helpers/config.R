# ---------------------------------------------------------------------
# Edit these two paths to match your machine.
# (Optional: leave as-is and instead set the environment variables
#  WORKFLOWS_CODE / WORKFLOW_DATA in ~/.Renviron to override them.)
# ---------------------------------------------------------------------

# Where the workflow code lives (the folder containing helpers/, 01_.../, ...)
WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE",
                            "/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows")

# Where the data lives (downloaded/deposited separately from the code)
BASE_DIR <- Sys.getenv("WORKFLOW_DATA",
                       "/home/grigoropoulou/Documents/Postdoc/projects/workflow_paper/data")
