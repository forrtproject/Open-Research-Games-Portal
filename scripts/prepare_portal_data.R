library(readxl)
library(dplyr)  
library(tidyr) 
library(googlesheets4)
library(stringr)

# ----- Reading in data from both forms, anonimizing, and combining

# Read in data from New Entries form
google_sheet_url_ne <- "https://docs.google.com/spreadsheets/d/1GCInWJYzSmFEob4lKhdtFW6nnoGf_p5lWajdTE285v4/edit"
sheets_ne <- sheet_names(google_sheet_url_ne)
df_ne <- read_sheet(google_sheet_url_ne, sheet = sheets_ne)
df_ne_anonymous <- df_ne %>% select(2:27, `Entry ID`)  # Anonimize
#df_ne_anonymous <- df_ne_anonymous %>% mutate('Entry ID' = sprintf("ENTN_%03d", row_number()))

# Read in data from New Additions form
google_sheet_url_a <- "https://docs.google.com/spreadsheets/d/1nK_OBmuI7LtbpaHBmdWEbmbNMgQCpRDDtz3S10M4gVw/edit"
sheets_a <- sheet_names(google_sheet_url_a)
df_a <- read_sheet(google_sheet_url_a, sheet = sheets_a)
df_a_anonymous <- df_a %>% select(2:25, `Entry ID`) # Anonimize
#df_a_anonymous <- df_a_anonymous %>% mutate('Entry ID' = sprintf("ENTA_%03d", row_number()))

# Prepare data for combining dataframes
df_a_anonymous <- df_a_anonymous %>% 
  mutate(Creators = NA) %>% 
  mutate(Description = NA) %>% 
  select(1:2, Creators, Description, 3:27, `Entry ID`)
names(df_a_anonymous) == names(df_ne_anonymous) # Check if all correct

# Combine data
df_combined <- rbind(df_ne_anonymous, df_a_anonymous)

# ----- Data cleaning

# Remove explanations of Clusters
df_combined$`FORRT clusters` <-  str_replace_all(df_combined$`FORRT clusters`, c(
  "Cluster 1: Replication Crisis and Credibility Revolution - The background, events, and reforms associated with the replication crisis, including scientific misconduct, questionable research practices, and proposed solutions." = "Cluster 1: Replication Crisis and Credibility Revolution",
  "Cluster 2: Conceptual and Statistical Knowledge - Key concepts in statistics, measurement, and research design, including effect sizes, hypothesis testing, Bayesian reasoning, and validity concerns." = "Cluster 2: Conceptual and Statistical Knowledge",
  "Cluster 3: Ways of Working - Different ways research is organized and carried out, including big team science, community engagement, science communication, and researcher roles." = "Cluster 3: Ways of Working",
  "Cluster 4: Pre-analysis Planning - Pre-registration, registered reports, and other practices that involve specifying study methods and analyses before data collection to reduce bias." = "Cluster 4: Pre-analysis Planning",
  "Cluster 5: Transparency and Reproducibility in Computation and Analysis - Practices that make analysis reproducible and transparent, including scripted workflows, use of open-source tools, and good coding habits." = "Cluster 5: Transparency and Reproducibility in Computation and Analysis",
  "Cluster 6: FAIR Data and Materials - Making data and materials findable, accessible, interoperable, and reusable, including ethical sharing, metadata standards, and licensing." = "Cluster 6: FAIR Data and Materials",
  "Cluster 7: Publication Sharing - Open access models, preprints, peer review reform, and strategies for making research outputs more widely and freely available." = "Cluster 7: Publication Sharing",
  "Cluster 8: Replication and Meta-Research - Approaches to replication and meta-research, including study designs, challenges, and their role in evaluating the reliability of findings." = "Cluster 8: Replication and Meta-Research",
  "Cluster 9: Academic Structures and Institutions - How institutional policies, incentive systems, and academic culture influence openness, diversity, equity, and responsible research." = "Cluster 9: Academic Structures and Institutions",
  "Cluster 10: Qualitative Research - Open science practices tailored to qualitative methods, including transparency, reflexivity, ethical sharing, and standards of rigor." = "Cluster 10: Qualitative Research",
  "Cluster 11: Research Integrity - Issues related to ethical conduct in research, such as honesty, accountability, misconduct, and the promotion of trustworthy practices." = "Cluster 11: Research Integrity")
)

df_combined$Tonality <-  str_replace_all(df_combined$Tonality, c("Lighthearted/Playful - Fun-focused, good for engagement" = "Lighthearted/Playful",
                                                                 "Balanced - Mix of fun and learning" = "Balanced",
                                                                 "Learning-Intensive - Deep focus on concepts and understanding" = "Learning-intensive"))

# Distinguish Game & Entry ID's & clean formatting
Portal <- df_combined %>% rename('Game ID' = 'Unique ID', 'Tone' = 'Tonality')

# Read in data from author permission sheet
google_sheet_url_per <- "https://docs.google.com/spreadsheets/d/1RyAY2lHhLF83lJXi0FPm56nnrSpxS48e7C9La7NvfU8/edit?gid=0#gid=0"
sheets_per <- sheet_names(google_sheet_url_per)
df_per <- read_sheet(google_sheet_url_per, sheet = sheets_per)
# Save Entry ID's for entries with game author permissions
permissions <- df_per[df_per$Permission == 'Consent granted',] %>% drop_na()
permissions <- permissions$`Entry ID`

# Remove entries without game author permissions
Portal <- Portal %>% filter(.data[["Entry ID"]] %in% permissions)

# Order by Game-ID
Portal <- Portal[order(Portal$`Game ID`), ]


# ----- Combine entries of the same game in one row

# Create new dataframe & delete duplicate information for same game
df <- Portal %>%
  group_by(.data[["Game ID"]]) %>%
  mutate(across(4:26, ~ ifelse(duplicated(as.character(.x)), "", as.character(.x)) )) %>%
  ungroup()

# Create function for combining multiple entries on the same game
combine_cells <- function(x) {
  x <- x[!is.na(x) & x != ""]
  x_unique <- unique(x)
  
  if (length(x_unique) == 0) return("")
  if (length(x_unique) == 1) return(x_unique)
  paste(paste0("• ", x_unique), collapse = "\r\n")
}

# Create new dataframe & group data by Game ID and combine rows
collapsed <- df %>%
  group_by(.data[["Game ID"]]) %>%
  summarise(across(everything(), combine_cells), .groups = "drop")

# Write both versions to the Open Research Games Portal Google Sheet
sheet_write(collapsed, ss = ss, sheet = "Open Research Games Portal")
sheet_write(Portal, ss = ss, sheet = "Portal (Row per entry version)")