#!/usr/bin/env Rscript
# ==============================================================================
# check_github_projects_status.R
#
# Scans a directory tree for local Git repositories (including GitHub
# Enterprise clones, e.g. mskcc.org) and reports the status of each one:
#   - current branch
#   - clean / dirty working tree (uncommitted or untracked changes)
#   - commits ahead/behind the remote tracking branch
#   - date of last commit
#   - remote URL
#
# This checks LOCAL clones on disk via `git`. It does not call the GitHub
# API, so it won't show open PRs/issues/CI status — only local repo health
# (e.g. "you have uncommitted changes", "you're 3 commits behind origin").
#
# Requirements: git must be installed and on PATH. Tested with base R;
# uses dplyr/purrr if available for nicer output, falls back to base R.
# ==============================================================================

suppressWarnings(suppressMessages({
  has_tidy <- requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("purrr", quietly = TRUE)
  if (has_tidy) {
    library(dplyr)
    library(purrr)
  }
}))

# ---- Config -----------------------------------------------------------------

# Root folder(s) to search for git repos. Edit this to match your setup, e.g.
# "C:/Users/yourname/Documents" or "~/projects". Defaults to current dir.
search_roots <- c(".")

# How many directory levels deep to search for .git folders
max_depth <- 5

# ---- Helpers ------------------------------------------------------------

#' Find all directories containing a .git folder under given root(s)
find_git_repos <- function(roots, max_depth = 5) {
  all_repos <- character(0)

  for (root in roots) {
    if (!dir.exists(root)) {
      warning(sprintf("Search root does not exist: %s", root))
      next
    }

    # list.dirs is simplest/most portable way to walk the tree in base R
    all_dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)

    # Limit depth relative to root, to avoid crawling huge trees (e.g. node_modules)
    root_depth <- length(strsplit(normalizePath(root, mustWork = FALSE), "[/\\\\]")[[1]])
    keep <- vapply(all_dirs, function(d) {
      d_depth <- length(strsplit(normalizePath(d, mustWork = FALSE), "[/\\\\]")[[1]])
      (d_depth - root_depth) <= max_depth
    }, logical(1))
    all_dirs <- all_dirs[keep]

    git_dirs <- all_dirs[basename(all_dirs) == ".git"]
    repo_dirs <- dirname(git_dirs)
    all_repos <- c(all_repos, repo_dirs)
  }

  unique(normalizePath(all_repos, mustWork = FALSE))
}

#' Run a git command inside a given repo directory and return trimmed stdout
git_cmd <- function(repo_path, args) {
  out <- tryCatch(
    system2("git", args = c("-C", shQuote(repo_path), args),
            stdout = TRUE, stderr = TRUE),
    error = function(e) NA_character_
  )
  status <- attr(out, "status")
  if (!is.null(status) && !is.na(status) && status != 0) {
    return(list(ok = FALSE, output = paste(out, collapse = "\n")))
  }
  list(ok = TRUE, output = paste(trimws(out), collapse = "\n"))
}

#' Get the status of a single repo as a one-row data frame
get_repo_status <- function(repo_path) {

  name <- basename(repo_path)

  branch  <- git_cmd(repo_path, c("rev-parse", "--abbrev-ref", "HEAD"))
  remote  <- git_cmd(repo_path, c("remote", "get-url", "origin"))
  dirty   <- git_cmd(repo_path, c("status", "--porcelain"))
  last_dt <- git_cmd(repo_path, c("log", "-1", "--format=%cd", "--date=iso-strict"))

  branch_name <- if (branch$ok) branch$output else NA_character_
  remote_url  <- if (remote$ok) remote$output else NA_character_
  is_dirty    <- if (dirty$ok) nzchar(dirty$output) else NA
  last_commit <- if (last_dt$ok && nzchar(last_dt$output)) {
    suppressWarnings(as.POSIXct(last_dt$output, format = "%Y-%m-%dT%H:%M:%S"))
  } else {
    as.POSIXct(NA)
  }

  # Ahead/behind vs upstream tracking branch (if one is configured)
  ahead <- NA_integer_
  behind <- NA_integer_
  has_upstream <- git_cmd(repo_path, c("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"))
  if (has_upstream$ok) {
    counts <- git_cmd(repo_path, c("rev-list", "--left-right", "--count", "HEAD...@{u}"))
    if (counts$ok && nzchar(counts$output)) {
      parts <- as.integer(strsplit(counts$output, "\\s+")[[1]])
      if (length(parts) == 2) {
        ahead  <- parts[1]
        behind <- parts[2]
      }
    }
  }

  status_label <- dplyr_or_base_status(is_dirty, ahead, behind, has_upstream$ok)

  data.frame(
    project      = name,
    path         = repo_path,
    branch       = branch_name,
    remote_url   = remote_url,
    has_upstream = isTRUE(has_upstream$ok),
    uncommitted  = is_dirty,
    ahead        = ahead,
    behind       = behind,
    last_commit  = last_commit,
    status       = status_label,
    stringsAsFactors = FALSE
  )
}

#' Turn the raw signals into a single human-readable status label
dplyr_or_base_status <- function(is_dirty, ahead, behind, has_upstream) {
  if (is.na(is_dirty)) return("error reading status")

  bits <- character(0)
  if (isTRUE(is_dirty)) bits <- c(bits, "uncommitted changes")
  if (!isTRUE(has_upstream)) {
    bits <- c(bits, "no upstream tracking branch")
  } else {
    if (!is.na(ahead)  && ahead  > 0) bits <- c(bits, sprintf("%d ahead", ahead))
    if (!is.na(behind) && behind > 0) bits <- c(bits, sprintf("%d behind", behind))
  }

  if (length(bits) == 0) "clean & up to date" else paste(bits, collapse = "; ")
}

# ---- Main -----------------------------------------------------------------

check_all_projects <- function(roots = search_roots, max_depth = 5, verbose = TRUE) {
  repos <- find_git_repos(roots, max_depth = max_depth)

  if (length(repos) == 0) {
    message("No git repositories found under: ", paste(roots, collapse = ", "))
    return(invisible(data.frame()))
  }

  if (verbose) message(sprintf("Found %d git repo(s). Checking status...", length(repos)))

  results <- if (has_tidy) {
    purrr::map_dfr(repos, get_repo_status)
  } else {
    do.call(rbind, lapply(repos, get_repo_status))
  }

  results <- results[order(results$project), ]
  rownames(results) <- NULL
  results
}

# ---- Run when sourced/executed directly -------------------------------------

if (sys.nframe() == 0) {
  status_report <- check_all_projects(search_roots, max_depth)

  cat("\n===== Git Project Status Report =====\n\n")
  if (nrow(status_report) > 0) {
    print(
      status_report[, c("project", "branch", "status", "last_commit")],
      row.names = FALSE
    )

    flagged <- status_report[
      status_report$uncommitted %in% TRUE |
        (!is.na(status_report$behind) & status_report$behind > 0) |
        !status_report$has_upstream,
    ]
    if (nrow(flagged) > 0) {
      cat("\n---- Needs attention ----\n\n")
      print(flagged[, c("project", "path", "status")], row.names = FALSE)
    } else {
      cat("\nAll repos are clean and up to date.\n")
    }
  } else {
    cat("No repositories found.\n")
  }
}
