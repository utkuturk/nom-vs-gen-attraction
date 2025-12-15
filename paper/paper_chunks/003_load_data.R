file_path <- "../data/experiment/Data59participants.csv"
num_participants_initial <- 0
num_participants_final <- 0
num_excluded <- 0

if (file.exists(file_path)) {
    raw_lines <- readLines(file_path)
    data_lines <- raw_lines[!grepl("^#", raw_lines)]
    con <- textConnection(data_lines)
    data <- read.csv(
        con,
        header = FALSE,
        fill = TRUE,
        col.names = paste0("V", 1:30),
        stringsAsFactors = FALSE
    )
    close(con)

    # Calculate filler accuracy
    filler_data <- data %>%
        filter(V16 == "filler") %>%
        select(
            Part_ID = V13,
            Correct_Resp = V19,
            Participant_Response = V20
        ) %>%
        mutate(
            Is_Correct = ifelse(Participant_Response == Correct_Resp, 1, 0)
        ) %>%
        group_by(Part_ID) %>%
        summarise(accuracy = mean(Is_Correct, na.rm = TRUE))

    bad_participants <- filler_data %>%
        filter(accuracy < 0.80) %>%
        pull(Part_ID)
    num_excluded <- length(bad_participants)

    clean_data <- data %>%
        filter(V16 == "experimental") %>%
        select(
            Part_ID = V13,
            Item_Set = V14,
            Trial_Order = V15,
            Condition1 = V17,
            Condition2 = V18,
            Correct_Resp = V19,
            Participant_Response = V20,
            RT = V21
        ) %>%
        distinct(Part_ID, Item_Set, Trial_Order, .keep_all = TRUE) %>%
        filter(!Part_ID %in% bad_participants) %>% # Exclude bad participants
        mutate(
            RT = as.numeric(as.character(RT)),
            Response_Binary = ifelse(Participant_Response == "Yes", 1, 0),
            Is_Correct = ifelse(Participant_Response == Correct_Resp, 1, 0),
            Condition1 = as.factor(Condition1),
            Condition2 = as.factor(Condition2)
        )

    num_participants_initial <- length(unique(data$V13)) # Subtract 1 if header/logic issue, or just unique IDs
    num_participants_final <- length(unique(clean_data$Part_ID))
    num_items <- length(unique(clean_data$Item_Set))
} else {
    clean_data <- data.frame()
}
