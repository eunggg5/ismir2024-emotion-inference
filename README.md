# ismir2024-emotion-inference

# Emotion Inference Analysis with Linear Mixed Effects (LME) Models

This repository contains scripts for analyzing emotion inference using Linear Mixed Effects (LME) models. The analysis focuses on dynamic and discrete emotional responses in music perception, aiming to identify predictors of listeners’ emotional ratings based on audio features and composers' intentions.

## Project Overview
In this project:

- *Observer* refers to the listener who perceives and rates the emotional valence of the music.
- *Target* refers to the composer who intended the emotional expression during the music performance.

## Related Publication
This study and analysis are based on our recent publication:

E.J. Oh, H. Kim, and K.M. Lee, “Which audio features can predict the dynamic musical emotions of both composers and listeners?” in *Proceedings of the 25th International Society for Music Information Retrieval Conference*, San Francisco, United States, 2024.

### Abstract
Are composers’ emotional intentions conveyed to listeners through audio features? In the field of Music Emotion Recognition (MER), recent efforts have been made to predict listeners’ (observers') time-varying perceived emotions using machine-learning models. However, interpreting these models has been challenging due to their black-box nature. To increase the explainability of models for subjective emotional experiences, we focus on composers’ (targets') emotional intentions.

Our study aims to determine which audio features effectively predict both composers’ time-varying emotions and listeners’ perceived emotions. Seven composers performed 18 piano improvisations expressing three types of emotions (joy/happiness, sadness, and anger), which were then listened to by 36 participants in a laboratory setting. Both composers and listeners continuously assessed the emotional valence of the music clips on a 9-point scale (1: ‘*very negative*’ to 9: ‘*very positive*’).

Linear mixed-effect models analysis revealed that listeners significantly perceived the composers’ intended emotions. Regarding audio features, the RMS was found to modulate the degree to which the listener’s perceived emotion resembled the composer’s emotion across all emotions. Moreover, the significant audio features that influenced this relationship varied depending on the emotion type. We propose that audio features related to the emotional responses of composers and listeners can be considered key factors in predicting listeners’ emotional responses.

## Data Requirements

Due to data privacy, certain essential data files are not included in this repository. You need to obtain or generate separately:

- **`observer_rate_2Hz.csv`**: Contains observer (listener) emotional ratings sampled at 2Hz.
- **`target_rate_2Hz.csv`**: Contains target (composer) emotional ratings sampled at 2Hz.
- **`mir_features_240322/`**: Folder containing pre-calculated Music Information Retrieval (MIR) features, including files such as `rms.csv`, `flatness.csv`, `zerocross.csv`, etc.

### How to Obtain or Generate the Missing Data

#### 1. Emotional Ratings Files (`observer_rate_2Hz.csv`, `target_rate_2Hz.csv`)

- **Option 1**: Obtain these files directly from the data provider or research team responsible for collecting and processing emotional ratings.
- **Option 2**: If you have the raw data, you can preprocess it by sampling ratings at 2Hz and saving the results in `.csv` format.

#### 2. MIR Features (`mir_features_240322/`)

To generate the MIR features, you can use an audio analysis toolkit, such as Python’s `librosa`, to compute features like RMS, zero-crossing rate, spectral centroid, etc., for each audio stimulus. Save the generated features as individual `.csv` files within a folder named `mir_features_240322/`.

Example Python Code to Generate MIR Features:

```python
import librosa
import pandas as pd
import os

def extract_mir_features(audio_path, output_folder):
    y, sr = librosa.load(audio_path)
    features = {
        "rms": librosa.feature.rms(y=y).mean(),
        "flatness": librosa.feature.spectral_flatness(y=y).mean(),
        "zerocross": librosa.feature.zero_crossing_rate(y=y).mean(),
        "centroid": librosa.feature.spectral_centroid(y=y, sr=sr).mean(),
        "rolloff": librosa.feature.spectral_rolloff(y=y, sr=sr).mean(),
    }
    file_name = os.path.splitext(os.path.basename(audio_path))[0]
    pd.DataFrame([features]).to_csv(f"{output_folder}/{file_name}.csv", index=False)

# Run this function for each audio file and save results in mir_features_240322/
```

### Directory Structure

Once you have obtained or generated these files, ensure they are placed in the following structure:

```
|-- main directory/
    |-- observer_rate_2Hz.csv
    |-- target_rate_2Hz.csv
    |-- mir_features_240322/
        |-- mir_rms.csv
        |-- mir_flatness.csv
        |-- mir_zerocross.csv
        |-- mir_centroid.csv
        |-- mir_rolloff.csv
```

## Script Overview

Each script in the repository serves a specific purpose in the analysis pipeline. Below is an overview of the main scripts and their functions:

1. **`setup.R`**: Loads necessary libraries and sets up the working directory.

2. **`data_preparation.R`**: Prepares the data by importing and cleaning the emotional ratings and MIR features, and structures the dataset for analysis.

3. **`discrete_LME.R`**: Conducts Linear Mixed Effects modeling on discrete ratings (single-time emotional judgments) to identify predictors.

4. **`discrete_cross_validation.R`**: Implements cross-validation on discrete LME models to validate the performance and generalizability of the model across different subsets of data.

5. **`dynamic_LME_total.R`**: Compares LME models on dynamic ratings (continuous-time emotional judgments) to examine predictors of emotional changes.

6. **`dynamic_LME_joy_happiness.R`, `dynamic_LME_sadness.R`, `dynamic_LME_anger.R`**: Run dynamic LME analyses specifically focused on each individual emotion (joy/happiness, sadness, and anger).

7. **`master_script.R`**: The main script that sources each analysis module sequentially. Running this script executes the entire analysis pipeline, assuming all required data files are available.

## Additional Information

The repository is structured to facilitate reproducible research. If you have access to the necessary data files, simply place them in the appropriate directories as outlined above, and run `master_script.R` to execute the full analysis.

## Sample Usage

1. Set up your working directory by running:

    ```r
    setwd("/path/to/your/directory")
    ```

2. Source the `master_script.R` to execute the entire analysis:

    ```r
    source("master_script.R")
    ```

3. View results in the console or generated output files.

## ratings_interpolation.py
The `ratings_interpolation.py` script processes and interpolates time-series emotion rating data for *target* (composers' ratings) and *observer* (listeners' ratings). The data is upsampled to 2Hz (0.5-second intervals) for consistent temporal alignment.

### Script Details
#### Functions

- `interpolate_values(time, value, length, interval=0.5)`:
    - Takes in lists of time points and corresponding rating values, creating a uniformly spaced time array using linear interpolation.
    - **Parameters**:
        - `time`: Time points (in seconds) where ratings were recorded.
        - `value`: Corresponding rating values.
        - `length`: Total length of the observation period.
        - `interval`: Interval at which to interpolate values (default is 0.5 seconds for 2Hz).
    - **Returns**: Interpolated time points and values.

- `process_data(file_path, output_file, columns)`:
    - Reads an input CSV file, applies interpolation to each row, and saves results to a new CSV.
    - **Parameters**:
        - `file_path`: Path to the input CSV.
        - `output_file`: Path for saving the output CSV with interpolated data.
        - `columns`: Columns to include in the output.
    - **Process**:
        - Converts `time` and `score` columns from strings to lists, `interpolates values`, and stores results.
        - Saves processed data to a new CSV file.

#### Usage
The script processes two datasets:

- **Target Dataset (Composers' Ratings)**: `target_rate.csv` → `target_rate_2Hz.csv`
- **Observer Dataset (Listeners' Ratings)**: `observer_videorate.csv` → `observer_rate_2Hz.csv`

To run this script, ensure the input files are in the main directory. The script generates `target_rate_2Hz.csv` and `observer_rate_2Hz.csv` as outputs for further analysis.

## License

This repository is released under the MIT License.