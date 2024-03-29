## ---
##
## Script name: barela_etal_2023_rcode.R
##
## Purpose of script: Analyze dog impulsivity systematic review and meta-analysis data
##
## Authors: Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2022-08-17
##
## Date Finalized: 2023-01-05
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licencor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
## ---


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

library(papaja)
library(PRISMA2020)
library(RoBMA)
library(robvis)


# Define functions ----------------------------------------------------------

# Remove repeated vector entries to do the opposite of the dplyr::fill() function
unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}


#  Import and determine unique records -----------------------------------

# Import and combine records from three sheets within an Excel spreadsheet
search_file <- "barela_etal_2024_data1.xlsx"
psychinfo <- read_xlsx(search_file, sheet = "psychinfo") |>
  mutate(year = year(parse_date(as.character(`Publication Date`), format = "%Y%m%d"))) |>
  mutate(across(everything(), as.character)) |>
  select(author = Author, year, title = `Article Title`, journal = `Journal Title`, volume = Volume, issue = Issue, page = `First Page`, abstract = Abstract, doi = DOI) |>
  arrange(doi)
scopus <- read_xlsx(search_file, sheet = "scopus") |>
  mutate(across(everything(), as.character)) |>
  select(author = Authors, year = Year, title = Title, journal = `Source title`, volume = Volume, issue = Issue, page = `Page start`, abstract = Abstract, doi = DOI)
wos <- read_xlsx(search_file, sheet = "webofscience") |>
  mutate(across(everything(), as.character)) |>
  select(author = Authors, year = `Publication Year`, title = `Article Title`, journal = `Source Title`, volume = Volume, issue = Issue, page = `Start Page`, abstract = Abstract, doi = DOI) |>
  mutate(volume = as.character(volume))
all_records <- bind_rows(psychinfo, scopus, wos)
n_total_records <- nrow(all_records)

# Find unique records
unique_dois <- all_records |>
  drop_na(doi) |>
  distinct(doi)
no_doi <- all_records |>
  filter(is.na(doi))
unique_records <- bind_rows(unique_dois, no_doi)
n_unique_records <- nrow(unique_records)


# Import and process reviewed records ------------------------------------

reviewed_studies <- read_csv("barela_etal_2024_data2.csv", show_col_types = FALSE) |>
  mutate(dog_type = str_to_sentence(dog_type),
         dog_type = gsub(" (sled, husky lineages)", "", dog_type, fixed = TRUE),
         dog_type = gsub(" (border collie)", "", dog_type, fixed = TRUE),
         entry = str_extract(author, '\\w*'),
         entry = sub("MÃ", "Mueller", entry),
         entry = paste0(entry, " et al."),
         entry = sub("Olsen et al.", "Olsen", entry),
         entry = paste0(entry, " (", year, ")"),
         entry = ifelse(entry == "Brady et al. (2018)" & is.na(sample_size), "Brady et al. (2018b)", entry),
         entry = ifelse(entry == "Fagnani et al. (2016)" & sample_size == 14, "Fagnani et al. (2016a)", entry),
         entry = ifelse(entry == "Fagnani et al. (2016)" & sample_size == 13, "Fagnani et al. (2016a)", entry),
         entry = ifelse(entry == "Fagnani et al. (2016)" & sample_size == 22, "Fagnani et al. (2016b)", entry),
         study = entry,
         sample_size = gsub(" (Exp. 1)", "", sample_size, fixed = TRUE),
         sample_size = gsub(" (Exp. 2)", "", sample_size, fixed = TRUE),
         sample_size = as.numeric(sample_size),
         neutered_status = na_if(neutered_status, "N/A"),
         tasks = sub("bucket", "cup", tasks),
         tasks = sub("delayed discounting", "delay discounting", tasks),
         tasks = sub("delay of gratification", "delay discounting", tasks),
         tasks = sub("detour-fence", "detour fence", tasks),
         dias = case_when(
           if_any(contains("correlate_"), ~ str_detect(.x, "DIAS")) ~ 1,
           TRUE ~ 0
         )
  ) |>
  mutate(across(contains("correlate_"), ~ sub("bucket", "cup", .x))) |>
  mutate(across(contains("correlate_"), ~ sub("detour-fence", "detour fence", .x))) |>
  mutate(across(contains("correlate_"), ~ sub("delayed discounting", "delay discounting", .x))) |>
mutate(across(contains("correlate_"), ~ sub("delay of gratification", "delay discounting", .x)))

# Add study numbers to multi-study reports
reviewed_studies$study[which(reviewed_studies$study == "Olsen (2019)" & reviewed_studies$sample_size == 15)] <- "Olsen (2019) Study 1"
reviewed_studies$study[which(reviewed_studies$study == "Olsen (2019)" & reviewed_studies$sample_size == 34)] <- "Olsen (2019) Study 2"
reviewed_studies$study[which(reviewed_studies$study == "Fagnani et al. (2016a)" & reviewed_studies$sample_size == 14)] <- "Fagnani et al. (2016a) Study 1"
reviewed_studies$study[which(reviewed_studies$study == "Fagnani et al. (2016a)" & reviewed_studies$sample_size == 13)] <- "Fagnani et al. (2016a) Study 2"
reviewed_studies$dog_type[which(reviewed_studies$study == "Fagnani et al. (2016b)" & reviewed_studies$sample_size == 22)] <- "Pet/ Shelter"
reviewed_studies$study[which(reviewed_studies$study == "Brady et al. (2018)" & reviewed_studies$sample_size == 24)] <- "Brady et al. (2018)  Study 1"
reviewed_studies$study[which(reviewed_studies$study == "Brady et al. (2018)" & reviewed_studies$sample_size == 13)] <- "Brady et al. (2018)  Study 2"
reviewed_studies$study[which(reviewed_studies$study == "Brady et al. (2018)" & reviewed_studies$sample_size == 23)] <- "Brady et al. (2018)  Study 3"
reviewed_studies$study[which(reviewed_studies$study == "Marshall et al. (2015)" & reviewed_studies$sample_size == 14)] <- "Marshall-Pescini et al. (2015)"

# Extract unique records
reviewed_records <- reviewed_studies |>
  distinct(entry, .keep_all = TRUE)

# Extract analyzed studies
analyzed_studies <- reviewed_studies |>
  filter(exclusion == 0) |>
  select(study, sample_size:dias, entry) |>
  arrange(study) |>
  mutate(tasks = sub("delayed discounting", "delay discounting", tasks),
         tasks = ifelse(dias == 1, paste0(tasks, ", DIAS"), tasks)) |>
  mutate(study_num = row_number(), .before = 1)

# Extract analyzed records
analyzed_records <- analyzed_studies |>
  distinct(entry, .keep_all = TRUE) |>
  select(entry, dias)

# Extract all pairs of measures
measure_pairs_orig <- reviewed_studies |>
  mutate(exclusion = ifelse(is.na(exclusion), excluded, exclusion)) |>
  filter(exclusion == 0) |>
  select(study, sample_size:neutered_status, `correlate_a-1`:`reported_significant-17`) |>
  mutate(across(contains("-"), as.character)) |>
  pivot_longer(cols = contains("-"), names_to = "measure", values_to = "value") |>
  filter(!is.na(value)) |>
  separate(measure, into = c("measure", "comparison"), sep = "-") |>
  pivot_wider(id_cols = c(study:neutered_status, comparison), names_from = measure, values_from = value) |>
  arrange(correlate_a, correlate_b) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "trials before correct", "Number of trials before correct"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "# of trials before success", "Number of trials before correct"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "1st", "First"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "freq.", "Frequency"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "max.", "Maximum"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "\\% of choices of S\\+ in test phase", "Percent choosing larger"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "success", "Percent correct trials"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "perseveration", "Number of trials before correct"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "accuracy", "Percent correct trials"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "Maximum delay reached in seconds", "Maximum delay tolerated"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "Maximumdelay reached", "Maximum delay tolerated"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "Prate", "Rate of response during delay"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "duration", "Duration of time near fence"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "latency", "Latency to reward"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "breaks", "Number of breaks"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "break interval", "Interval between breaks"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "first break", "Time until first break"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "difference score", "Difference in choices between control/experimental"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "proximity", "Proximity"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "performance score", "Performance score"))) |>
  mutate(across(contains("correlate_"), ~str_replace(.x, "ratio correct choices in control/experimental", "Ratio correct choices in control/experimental"))) |>
  mutate(effectsize_value = as.numeric(effectsize_value)) |>
  arrange(study)

# Assign vector of measures whose scale needs to be reversed
reverse_measures <- c("social inhibition (Difference in choices between control/experimental)",
                      "a-not-b cup (First location searched)",
                      "sit-stay (Interval between breaks)",
                      # "delay of gratification (Maximum delay tolerated)",
                      "delay discounting (Maximum delay tolerated)",
                      "spatial impulsivity (Maximum distance travelled)",
                      "spatial impulsivity (Percent choosing larger)",
                      "cylinder (Percent correct trials)",
                      "leash (Performance score)",
                      "middle cup (Performance score)",
                      "wait-for-treat (Performance score)",
                      "middle cup (Ratio correct choices in control/experimental)",
                      "sit-stay (Time until first break)")

# Reverse scales of reverse_measures
measure_pairs <- measure_pairs_orig |>
  mutate(effectsize = ifelse(correlate_a %in% reverse_measures, -1 * effectsize_value, effectsize_value),
         effectsize = ifelse(correlate_b %in% reverse_measures, -1 * effectsize, effectsize))

# Extract single measure for each task
measure_pairs_trimmed <- measure_pairs |>
  filter(correlate_b != "cylinder (Frequency of errors)" &
           correlate_a != "delay discounting (Rate of response during delay)" &
           correlate_b != "detour fence (Duration of time near fence)" &
           correlate_a != "sit-stay (Interval between breaks)" &
           correlate_b != "sit-stay (Interval between breaks)" &
           correlate_a != "sit-stay (Time until first break)" &
           correlate_b != "sit-stay (Time until first break)" &
           (study != "Olsen (2019) Study 1" | correlate_a != "cylinder (Percent correct trials)") &
           (study != "Olsen (2019) Study 2" | correlate_a != "cylinder (Percent correct trials)") &
           (study != "Olsen (2019) Study 1" | correlate_b != "cylinder (Percent correct trials)") &
           (study != "Olsen (2019) Study 2" | correlate_b != "cylinder (Percent correct trials)")
  )

# Extract pairs with DIAS measures
dias_measure_pairs <- measure_pairs |>
  filter(grepl("DIAS", correlate_b))
dias_pairs <- measure_pairs_trimmed |>
  filter(grepl("DIAS", correlate_b)) |>
  mutate(task_a = str_replace(correlate_a, " \\s*\\([^\\)]+\\)", ""),
         task_a = str_to_title(task_a),
         task_b = correlate_b) |>
  mutate(across(contains("task_"), ~ gsub("Of", "of", .x)),
         across(contains("task_"), ~ gsub("-Not-", "-not-", .x)),
         across(contains("task_"), ~ gsub("-For-", "-for-", .x)))
dias_pairs_count <- dias_pairs |>
  count(reported_significant)

# Extract task/task pairs
task_pairs_all <- measure_pairs |>
  filter(!grepl("DIAS", correlate_b))
task_pairs <- measure_pairs_trimmed |>
  filter(!grepl("DIAS", correlate_b)) |>
  mutate(task_a = str_replace(correlate_a, " \\s*\\([^\\)]+\\)", ""),
         task_b = str_replace(correlate_b, " \\s*\\([^\\)]+\\)", ""),
         task_a = str_to_title(task_a),
         task_b = str_to_title(task_b),
         dog_type = str_to_sentence(dog_type),
         dog_type = sub(" (sled, husky lineages)", "", dog_type, fixed = TRUE),
         dog_type = sub("border collie", "Border collies only", dog_type)
  ) |>
  mutate(across(contains("task_"), ~ gsub("Of", "of", .x)),
         across(contains("task_"), ~ gsub("-Not-", "-not-", .x)),
         across(contains("task_"), ~ gsub("-For-", "-for-", .x))) |>
  arrange(study)
task_pairs_count <- task_pairs |>
  count(reported_significant)

# Extract task/DIAS pairs
task_dias_pairs <- bind_rows(task_pairs, dias_pairs) |>
  left_join(select(analyzed_studies, study_num, study), by = "study") |>
  mutate(effect_size = paste0(ifelse(effectsize_type == "spearman's correlation", "$\\rho", "$r"), "_{(", sample_size, ")}$=", printnum(as.numeric(effectsize), digits = 2), " [", study_num, "]"),
         effect_size = ifelse(reported_significant == "Yes", paste0("\\textbf{", effect_size, "*}"), effect_size)) |>
  arrange(study_num)
task_dias_pairs_count <- task_dias_pairs |>
  count(reported_significant)

# Create vector of tasks
tasks <- task_dias_pairs |>
  select(contains("task_")) |>
  pivot_longer(everything(), names_to = "correlate", values_to = "task") |>
  mutate(task = str_replace(task, " \\s*\\([^\\)]+\\)", "")) |>
  distinct(task) |>
  arrange(task) |>
  pull(task)


# Generate figures -----------------------------------------------------------------

# Create data frame PRISMA information
prisma_csv <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
prisma_df <- read_csv(prisma_csv, show_col_types = FALSE)
prisma_df$boxtext[which(prisma_df$node == "node6")] <- "Identification of new studies via databases"
prisma_df$n[which(prisma_df$node == "node6")] <- NA
prisma_df$n[which(prisma_df$data == "register_results")] <- NA
prisma_df$n[which(prisma_df$data == "excluded_automatic")] <- NA
prisma_df$n[which(prisma_df$data == "excluded_other")] <- NA
prisma_df$n[which(prisma_df$data == "new_reports")] <- NA
prisma_df$n[which(prisma_df$data == "database_results")] <- n_total_records
prisma_df$n[which(prisma_df$data == "database_specific_results")] <- paste0("PsychINFO,", nrow(psychinfo), "; Scopus,", nrow(scopus), "; Web of Science,", nrow(wos))
prisma_df$n[which(prisma_df$data == "duplicates")] <- n_total_records - n_unique_records
prisma_df$n[which(prisma_df$data == "records_screened")] <- n_unique_records
prisma_df$n[which(prisma_df$data == "records_excluded")] <- n_unique_records - nrow(reviewed_records)
prisma_df$n[which(prisma_df$data == "dbr_sought_reports")] <- nrow(reviewed_records)
prisma_df$n[which(prisma_df$data == "dbr_assessed")] <- nrow(reviewed_records)
prisma_df$n[which(prisma_df$data == "dbr_excluded")] <- paste0("Not original research paper in English,", nrow(filter(reviewed_records, exclusion == 1)), "; Did not include two impulsivity tasks,", nrow(filter(reviewed_records, exclusion == 2)), "; Did not report necessary \nstatistics,", nrow(filter(reviewed_records, exclusion == 3)))
prisma_df$n[which(prisma_df$data == "new_studies")] <- nrow(analyzed_records)
prisma_df$boxtext[which(prisma_df$data == "dbr_sought_reports")] <- "Articles sought for retrieval"
prisma_df$boxtext[which(prisma_df$data == "dbr_notretrieved_reports")] <- "Articles not retrieved"
prisma_df$boxtext[which(prisma_df$data == "dbr_assessed")] <- "Articles assessed for eligibility"
prisma_df$boxtext[which(prisma_df$data == "dbr_excluded")] <- "Articles excluded"
prisma_df$boxtext[which(prisma_df$data == "new_studies")] <- "Articles included in review"

# Create PRISMA flowchart
prisma_data <- PRISMA_data(prisma_df)
prisma_plot <- PRISMA_flowdiagram(prisma_data,
                                  fontsize = 14,
                                  detail_databases = TRUE,
                                  interactive = FALSE,
                                  previous = FALSE,
                                  other = FALSE)
PRISMA_save(prisma_plot, "figures/prisma_chart.png", filetype = "PNG", overwrite = TRUE)


# Generate tables ------------------------------------------------------------

# Generate table of studies, demographic info, and tasks
studies_table <- analyzed_studies |>
  ungroup() |>
  mutate(tasks = str_to_title(tasks),
         tasks = gsub("Dias", "DIAS", tasks),
         # tasks = gsub("Of", "of", tasks),
         tasks = gsub("-Not-", "-not-", tasks),
         tasks = gsub("-For-", "-for-", tasks),
         study = sub("Mueller", 'M\\\\"{u}ller', study)) |>
  select(study_num, study, sample_size:neutered_status, tasks) |>
  arrange(study_num)

# Generate table of tasks, associated measures, and studies
task_descriptions <- data.frame(task = tasks[-which(tasks == "DIAS")], desc = c("Dog is trained to travel around one side of barrier for reward and then must switch to the other side", "Dog is trained to select one cup with reward under it and then must switch to selecting another cup", "Dog has to find open side of transparent box to acquire reward", "Dogs must turn away from visible but inaccessible reward to press buzzer and acquire reward", "Dog is trained to obtain reward by entering either end of opaque cylinder and then must do the same with a transparent cylinder", "Dogs must choose between an immediate, smaller or lower quality reward vs. a delayed, larger or higher quality reward", "Dog could see reward behind transparent wall but must travel around wall to acquire reward", "Dog's leash was caught on obstacle, so they had to move away from owner to get to owner and reward", "Dog can knock over two of three cups to get rewards, which are only available in outer two cups", "Dog is commanded to `sit' and `stay' for ten minutes", "Dogs choose between an experimenter with more rewards who does not give them and an experimenter with fewer rewards who does give them", "Dogs must choose between smaller, closer and larger, more distant rewards", "Dogs is commanded to `wait' while a treat is placed in front of them"))
task_table <- measure_pairs |>
  pivot_longer(contains("correlate_"), names_to = "which", values_to = "task_measure") |>
  filter(task_measure != "DIAS") |>
  separate(task_measure, into = c("task", "measure"), sep = " \\(") |>
  select(task, measure, study) |>
  mutate(study = sub("Mueller", 'M\\\\"{u}ller', study),
         task = str_to_title(task),
         task = gsub("Of", "of", task),
         task = gsub("-Not-", "-not-", task),
         task = gsub("-For-", "-for-", task),
         measure = sub("\\)", "", measure),
         measure = sub("Interval between breaks", "Interval between breaks*", measure),
         measure = sub("Time until first break", "Time until first break*", measure),
         measure = sub("Difference in choices between control/experimental", "Difference in choices between control/experimental*", measure),
         measure = sub("First location searched", "First location searched*", measure),
         measure = sub("Percent correct trials", "Percent correct trials*", measure),
         measure = sub("Maximum delay tolerated", "Maximum delay tolerated*", measure),
         measure = sub("Performance score", "Performance score*", measure),
         measure = sub("Ratio correct choices in control/experimental", "Ratio correct choices in control/experimental*", measure),
         measure = sub("Maximum distance travelled", "Maximum distance travelled*", measure),
         measure = sub("Percent choosing larger", "Percent choosing larger*", measure)
  ) |>
  distinct(study, task, measure) |>
  arrange(task, measure, study) |>
  left_join(select(studies_table, study_num, study), by = "study") |>
  select(-study) |>
  group_by(task, measure) |>
  mutate(study = paste0(study_num, collapse = ", ")) |>
  select(-study_num) |>
  distinct() |>
  left_join(task_descriptions, by = "task") |>
  relocate(desc, .after = task)
task_table$study[which(task_table$measure == "Frequency of errors" & task_table$task == "A-not-B Cup")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Frequency of errors" & task_table$task == "A-not-B Cup")], "}")
task_table$study[which(task_table$measure == "Frequency of errors" & task_table$task == "Cylinder")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Frequency of errors" & task_table$task == "Cylinder")], "}")
task_table$study[which(task_table$measure == "Percent correct trials*")] <- paste0("4, 11, \\sout{14, 15}")
task_table$study[which(task_table$measure == "Rate of response during delay")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Rate of response during delay")], "}")
task_table$study[which(task_table$measure == "Duration of time near fence")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Duration of time near fence")], "}")
task_table$study[which(task_table$measure == "Interval between breaks*")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Interval between breaks*")], "}")
task_table$study[which(task_table$measure == "Time until first break*")] <- paste0("\\sout{", task_table$study[which(task_table$measure == "Time until first break*")], "}")

task_table <- task_table |>
  group_by(task, desc) |>
  summarise(measure = paste0(measure, collapse = "\n"), study = paste0(study, collapse = "\n"))

# task_table$task <- unfill_vec(task_table$task)
# task_table$desc <- unfill_vec(task_table$desc)
# task_table2 <- task_table |>
#   group_by(task, desc) |>
#   summarise(measure = paste0(measure, collapse = ", "))

task_dias_pairs_table <- task_dias_pairs |>
  group_by(task_a, task_b) %>%
  mutate(cell = paste0(effect_size, collapse = ",\n")) |>
  distinct(cell) |>
  bind_rows(data.frame(task_a = c(tasks, "DIAS"), task_b = c(tasks, "DIAS"), cell = NA)) |>
  ungroup() |>
  complete(task_a, task_b) |>
  pivot_wider(id_cols = task_a, names_from = task_b, values_from = cell) |>
  mutate(across(-task_a, ~ gsub("-", "$-$", .x))) |>
  relocate(DIAS, .after = 15) |>
  filter(task_a != "DIAS")


# Meta-analysis -----------------------------------------------------------

# Extract A-not-B/cylinder data
anotb_cylinder_data <- task_pairs |>
  filter(grepl("a-not-b cup", correlate_a) & grepl("cylinder", correlate_b)) |>
  mutate(n = as.numeric(str_sub(n, 1, 2)),
         study = paste0(study, " [N=", n, "]")
  ) |>
  select(study, r = effectsize_value, n, contains("rob_"))

# Set up cores for parallel processing
RoBMA.options(max_cores = parallel::detectCores() - 1)
RoBMA.get_option("max_cores")

# Conduct robust Bayesian meta-analysis
fit <- RoBMA(r = anotb_cylinder_data$r, n = anotb_cylinder_data$n, study_names = anotb_cylinder_data$study, seed = 1, parallel = TRUE)
(sum_fit <- summary(fit, output_scale = "r"))

# Extract interpretation of results
metaanalysis_text <- interpret(fit, output_scale = "r")
metaanalysis_text <- sub(". Robust Bayesian meta-analysis found moderate evidence against the publication bias", " and moderate evidence against publication bias", metaanalysis_text)
metaanalysis_text <- sub("BF_10", "$BF_{10}$", metaanalysis_text)
metaanalysis_text <- sub("BF\\^rf", "$BF_{10}$", metaanalysis_text)
metaanalysis_text <- sub("BF_pb", "$BF_{10}$", metaanalysis_text)
pet_mean <- sum_fit$estimates$Mean[which(row.names(sum_fit$estimates) == "PET")]
pet_lowerci <- sum_fit$estimates$`0.025`[which(row.names(sum_fit$estimates) == "PET")]
pet_upperci <- sum_fit$estimates$`0.975`[which(row.names(sum_fit$estimates) == "PET")]
peese_mean <- sum_fit$estimates$Mean[which(row.names(sum_fit$estimates) == "PEESE")]
peese_lowerci <- sum_fit$estimates$`0.025`[which(row.names(sum_fit$estimates) == "PEESE")]
peese_upperci <- sum_fit$estimates$`0.975`[which(row.names(sum_fit$estimates) == "PEESE")]

# Create forest plot
png("figures/anotb-cylinder-forestplot.png", width = 1000, height = 500, pointsize = 22)
forest(fit, output_scale = "r")
dev.off()

# Create data frame with risk of bias info for each study
rob_data <- analyzed_studies |>
  select(study, sample_size) |>
  mutate(study = paste0(study, " [N=", sample_size, "]")) |>
  select(study) |>
  mutate(Sampling.frame = c("Low", "Low", "Some concerns", "Low", "Low", "Some concerns", "Low", "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Low", "Some concerns", "Low", "Some concerns", "Low", "Low"),
         Participant.recruitment = c("Some concerns", "Some concerns", "Some concerns", "Low", "Some concerns", "Low", "Some concerns", "Some concerns", "Some concerns", "Low", "Low", "Low", "Some concerns", "Low", "Low", "Low", "Low"),
         Acceptability.of.exclusion.rate = c("Some concerns", "Low", "Low", "Low", "Low", "Low", "Some concerns", "Low", "Low", "Low", "Low", "Low", "Some concerns", "Low", "Low", "Low", "Low"),
         Sufficiency.of.sample.size = c("High", "High", "High", "Some concerns", "Low", "High", "High", "High", "High", "High", "High", "Some concerns", "Some concerns", "High", "Some concerns", "Some concerns", "Some concerns"),
         Demographic.variables = c("Some concerns", "Some concerns", "Some concerns", "Some concerns", "Low", "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Low", "Some concerns", "Low", "Some concerns", "Low", "Low", "Some concerns", "Low"),
         Reliability.of.measurements = c("Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Some concerns", "Low", "Low", "Low", "Low", "Low"),
         Setting = c("Low", "Some concerns", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Some concerns", "Some concerns", "Low", "Low"),
         Data.management = c("Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low","Low",  "Low", "Low", "Low", "Low", "Low", "Low", "Low"),
         Overall = c("High", "High", "Some concerns", "Low", "Low", "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Some concerns", "Low", "Some concerns", "High", "Some concerns", "Low", "Low")
  ) |>
  select(study, Sampling.frame:Overall)

# Generate risk of bias plots
rob_summary(rob_data, tool = "Generic", overall = FALSE, weighted = FALSE, colour = c("#009e73", "#f0e442", "#d55e00", "#000000", "#111111"))
ggsave("figures/rob_summary.png", width = 8, height = 5)
rob_traffic_light(rob_data, tool = "Generic", overall = FALSE, colour = c("#009e73", "#f0e442", "#d55e00", "#000000", "#111111"))
ggsave("figures/rob_trafficlight.png", width = 8, height = 10)

# Generate histogram of study sample sizes
analyzed_studies |>
  ggplot(aes(x = sample_size)) +
  geom_histogram(binwidth = 10, fill = "#0072b2", color = "black") +
  geom_vline(xintercept = 25, linetype = 2) +
  geom_vline(xintercept = 50, linetype = 2) +
  labs(x = "Sample size", y = "Frequency") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        panel.grid = element_blank())
ggsave("figures/samplesizes.png", height = 4, width = 6)

# Save workspace for faster sourcing
# save.image(file = "barela_etal_2023.RData"


# Generate codebook -------------------------------------------------------

codebook_data <- reviewed_studies |>
  select(-entry, -study) |>
  mutate(author = gsub("MiklÃ³si, ÃdÃ¡m", "Miklosi, Adam", author),
         author = gsub("MiklÃ³si Ã\u0081", "Miklosi, Adam", author))

codebook_labels <- c("Row number from full data set", "Author name(s)", "Publication year", "Publication title", "Journal name", "Volume number", "Issue number", "Page", "Digital object identifier (DOI)", "Flag indicating reason for exclusion", "Flag indicating reason for exclusion", "Total sample size", "Type of dog included in sample (pet, shelter, working, captive)", "Dog sex ratio (males:females)", "Dog neuter status (neutered:intact)", "Tasks conducted in study", "Flag indicating whether DIAS was adminstered (0=No, 1=Yes)", rep(c("Task and measure for first task in correlation", "Task and measure for second task in correlation", "Type of effect size for correlation", "Value of effect size for correlation", "Sample size for correlation", "Flag indicating whether correlation was reported as signficant (0=No, 1=Yes)"), 17))

for (i in 1:ncol(codebook_data)) {
  attr(codebook_data[[i]], "shortDescription") <- codebook_labels[i]
}
dataReporter::makeCodebook(codebook_data, reportTitle = "Codebook for dog impulsivity systematic review data", file = "barela_etal_2024_codebook.Rmd", replace = TRUE)
