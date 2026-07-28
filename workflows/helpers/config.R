# ---------------------------------------------------------------------
# Paths. Set these once, either as environment variables in a .Renviron
# file (recommended -- keeps machine-specific paths out of the repo) or by
# editing the two fallbacks below.
#
#   WORKFLOWS_CODE  the folder containing helpers/, 01_biodiversity_data/, ...
#   WORKFLOW_DATA   the folder holding the data (kept separate from the code)
#
# Example .Renviron, placed in this repo's root or in your home directory:
#
#   WORKFLOWS_CODE=/path/to/hydrographr/workflows
#   WORKFLOW_DATA=/path/to/workflow_paper/data
#
# See .Renviron.example. Both can also be set per run, e.g.
#   WORKFLOW_DATA=/path/to/other/data Rscript 07_sdm/08_habitat_classification.R
# which is how alternative or test data directories are used without
# editing any code.
# ---------------------------------------------------------------------

WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "")   # or put your path here
BASE_DIR      <- Sys.getenv("WORKFLOW_DATA",  "")   # or put your path here

if (!nzchar(WORKFLOWS_DIR))
  stop("WORKFLOWS_CODE is not set. Add it to a .Renviron file (see ",
       ".Renviron.example) or edit helpers/config.R. It must point at the ",
       "folder containing helpers/ and the numbered module folders.",
       call. = FALSE)

if (!nzchar(BASE_DIR))
  stop("WORKFLOW_DATA is not set. Add it to a .Renviron file (see ",
       ".Renviron.example) or edit helpers/config.R. It must point at the ",
       "data directory; see README.md for the expected layout.",
       call. = FALSE)

if (!dir.exists(WORKFLOWS_DIR))
  stop("WORKFLOWS_CODE points at a folder that does not exist: ",
       WORKFLOWS_DIR, call. = FALSE)

if (!dir.exists(BASE_DIR))
  stop("WORKFLOW_DATA points at a folder that does not exist: ",
       BASE_DIR, call. = FALSE)
