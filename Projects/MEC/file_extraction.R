library(foreign)
library(tidyverse)
library(ggfortify)

# This R code file does the following things:
# 1. extract features on all the songs under the certain directory, and save
#    them into different files song by song
# 2. take the mean of all features on all songs, and combine them into one 
#    dataframe, perpare it to model fitting

# extract features
bextract_path <- normalizePath("H:/Rrelative/STAT499/marsyas/build/bin/bextract.exe", winslash = "/")

audio_dir <- normalizePath("H:/Rrelative/STAT499/marsyas/build/audio", winslash = "/")

output_dir <- normalizePath("H:/Rrelative/STAT499/marsyas/build/output", winslash = "/")

wav_files <- list.files(audio_dir, pattern = "\\.wav$", full.names = TRUE)

for (wav_file in wav_files) {
  file_base <- tools::file_path_sans_ext(basename(wav_file))
  
  output_arff <- file.path(output_dir, paste0(file_base, ".arff"))
  output_csv <- file.path(output_dir, paste0(file_base, ".csv"))
  
  command <- c(shQuote(wav_file), "-w", shQuote(output_arff))
  system2(bextract_path, args = command, wait = TRUE)
  
  data <- read.arff(output_arff)
  write.csv(data, file = output_csv, row.names = FALSE)
}


# combine them into one csv file
csv_files <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)

mean_df <- data.frame(ID = character(), stringsAsFactors = FALSE)

for (csv_file in csv_files) {
  data <- read.csv(csv_file, header=TRUE)
  
  numeric_data <- data[sapply(data, is.numeric)]
  mean <- colMeans(numeric_data)
  name <- tools::file_path_sans_ext(basename(csv_file))
  mean_row <- c(ID=name, as.list(mean))
  mean_df <- rbind(mean_df, mean_row)
}

# add arousal and valence

av_data <- read.csv("data/static_annotations_averaged_songs_1_2000.csv")
ids <- as.vector(mean_df["ID", ])
av_data <- av_data %>%
  filter(as.character(song_id) %in% mean_df[, "ID"])
av_data[,"song_id"] <- as.character(av_data[,"song_id"])
mean_df[,"ID"] <- as.character(mean_df[,"ID"])

df <- inner_join(av_data, mean_df, by=c("song_id" = "ID"))

colnames(df) <- c("id", "valence_mean", "valence_std", "arousal_mean", "arousal_std",
                 1:62)
df$valence_mean <- ((df$valence_mean-1)/4)-1
df$arousal_mean <- ((df$arousal_mean-1)/4)-1
df <- df %>%
  mutate(Radius = sqrt(valence_mean^2+arousal_mean^2),
         Angle = atan2(arousal_mean, valence_mean))

df <- df %>%
  mutate(Emotion = case_when(
    30 > Angle | Angle == 360 ~ "Excited",
    30 <= Angle & 60 > Angle ~ "Pleased", 
    60 <= Angle & 90 > Angle ~ "Happy",
    90 <= Angle & 120 > Angle ~ "Annoying",
    120 <= Angle & 150 > Angle ~ "Nervous",
    150 <= Angle & 180 > Angle ~ "Angry",
    180 <= Angle & 210 > Angle ~ "Sad",
    210 <= Angle & 240 > Angle ~ "Sleepy", 
    240 <= Angle & 270 > Angle ~ "Bored",
    270 <= Angle & 300 > Angle ~ "Relaxed",
    300 <= Angle & 330 > Angle ~ "Calm",
    330 <= Angle & 360 > Angle ~ "Peaceful"))

write.csv(df, file="1751_df.csv")
