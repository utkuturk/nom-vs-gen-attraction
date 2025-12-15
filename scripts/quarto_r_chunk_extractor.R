# taken from https://gist.github.com/utkuturk/3e688db557e1a21fcd0461836fdffe6d

split_chunks <- function(qmd_file) {
    # force the folder name to be based on the file name
    output_dir <- paste0(tools::file_path_sans_ext(qmd_file), "_chunks")

    # Make the folder (don't worry if it exists)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # read the lines
    lines <- readLines(qmd_file)

    # regex stuff to find the blocks
    chunk_header_pattern <- "^\\s*```\\{r(.*)\\}\\s*$"
    chunk_end_pattern <- "^\\s*```\\s*$"
    yaml_label_pattern <- "^\\s*#\\|\\s*label:\\s*(.+)\\s*$"
    source_pattern <- 'source\\s*\\(\\s*["\']([^"\']+)["\']'

    # variables to track where we are
    in_chunk <- FALSE
    chunk_buffer <- c()
    chunk_counter <- 1
    current_label <- ""

    for (i in seq_along(lines)) {
        line <- lines[i]

        # 1. found the start of a chunk?
        if (grepl(chunk_header_pattern, line, ignore.case = TRUE)) {
            in_chunk <- TRUE
            chunk_buffer <- c()

            # grab the stuff inside {r ...}
            header_content <- gsub(chunk_header_pattern, "\\1", line)
            parts <- strsplit(header_content, ",")[[1]]

            clean_label <- "unnamed"
            if (length(parts) > 0) {
                raw_label <- trimws(parts[1])
                # make sure it's a real label and not an option like echo=FALSE
                if (
                    !is.na(raw_label) &&
                        raw_label != "" &&
                        !grepl("=", raw_label)
                ) {
                    clean_label <- raw_label
                }
            }

            current_label <- clean_label
            next
        }

        # 2. found the end of a chunk?
        if (in_chunk && grepl(chunk_end_pattern, line)) {
            in_chunk <- FALSE

            # make the filename
            # Sanitize label for filename
            clean_label <- gsub("[^a-zA-Z0-9_-]", "_", current_label)
            filename <- sprintf("%03d_%s.R", chunk_counter, clean_label)
            filepath <- file.path(output_dir, filename)

            # save the file
            if (length(chunk_buffer) > 0) {
                writeLines(chunk_buffer, filepath)
            } else {
                file.create(filepath)
            }

            chunk_counter <- chunk_counter + 1
            chunk_buffer <- c()
            next
        }

        # 3. inside a chunk, so save the lines
        if (in_chunk) {
            # check if there is a #| label override
            if (grepl(yaml_label_pattern, line)) {
                extracted_label <- gsub(yaml_label_pattern, "\\1", line)
                current_label <- trimws(extracted_label)
                chunk_buffer <- c(chunk_buffer, line)
                next
            }

            # check if we need to expand a source() call
            if (grepl(source_pattern, line)) {
                matches <- regexec(source_pattern, line)
                parts <- regmatches(line, matches)[[1]]

                if (length(parts) >= 2) {
                    rel_path <- parts[2]
                    full_path <- file.path(dirname(qmd_file), rel_path)

                    if (file.exists(full_path)) {
                        external_code <- readLines(full_path)
                        chunk_buffer <- c(
                            chunk_buffer,
                            paste0("# >>> Expanded source: ", rel_path),
                            external_code,
                            paste0("# <<< End expansion: ", rel_path)
                        )
                    } else {
                        chunk_buffer <- c(
                            chunk_buffer,
                            paste0(
                                "# WARNING: Could not find source file: ",
                                full_path
                            )
                        )
                    }
                    next
                }
            }

            # just add the line
            chunk_buffer <- c(chunk_buffer, line)
        }
    }
}

# Execute the function
split_chunks("paper/paper.qmd")
