# ismir2024-emotion-inference

# Emotion Inference Analysis with Linear Mixed Effects (LME) Models

This repository contains scripts for analyzing emotion inference using Linear Mixed Effects (LME) models. The analysis focuses on dynamic and discrete emotional responses in music perception, aiming to identify predictors of listeners’ emotional ratings based on audio features and composers' intentions.

## Missing Data Files

Due to data privacy or size constraints, certain essential data files are not included in this repository and need to be obtained or generated separately:

- **`observer_rate_2Hz.csv`**: Contains observer emotional ratings sampled at 2Hz.
- **`target_rate_2Hz.csv`**: Contains target emotional ratings sampled at 2Hz.
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
        |-- rms.csv
        |-- flatness.csv
        |-- zerocross.csv
        |-- centroid.csv
        |-- rolloff.csv
```

## Error Handling for Missing Files

The scripts are designed to check for the existence of required files. If files are missing, an error message will prompt you to add them.

Example error handling in `setup.R`:

```r
if (!file.exists("observer_rate_2Hz.csv") || !file.exists("target_rate_2Hz.csv")) {
    stop("Error: observer_rate_2Hz.csv and/or target_rate_2Hz.csv are missing. Please add them to the main directory.")
}

if (!dir.exists("mir_features_240322")) {
    stop("Error: mir_features_240322 directory is missing. Please add it with necessary feature files.")
}
```

## Project Structure

Each script in the repository serves a specific purpose in the analysis pipeline. Below is an overview of the main scripts and their functions:

1. **`setup.R`**: Loads necessary libraries and sets up the working directory.

2. **`data_preparation.R`**: Prepares the data by importing and cleaning the emotional ratings and MIR features, and structures the dataset for analysis.

3. **`discrete_LME.R`**: Conducts Linear Mixed Effects modeling on discrete ratings (single-time emotional judgments) to identify predictors for different emotional states.

4. **`dynamic_LME_total.R`**: Compares LME models on dynamic ratings (continuous-time emotional judgments) to examine predictors of emotional changes.

5. **`dynamic_LME_joy_happiness.R`, `dynamic_LME_sadness.R`, `dynamic_LME_anger.R`**: Run dynamic LME analyses specifically focused on each individual emotion (joy, sadness, and anger).

6. **`discrete_cross_validation.R`**: Implements cross-validation on discrete LME models to validate the performance and generalizability of the model across different subsets of data.

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

3. View results in the console or in generated output files.

## License

This repository is released under the MIT License.


# Emotion Inference Analysis with Linear Mixed Effects (LME) Models

This repository contains scripts for analyzing emotion inference using Linear Mixed Effects (LME) models. The analysis focuses on dynamic and discrete emotional responses in music perception, aiming to identify predictors of listeners’ emotional ratings based on audio features and composers' intentions.

## Missing Data Files

Due to data privacy or size constraints, certain essential data files are not included in this repository and need to be obtained or generated separately:

- **`observer_rate_2Hz.csv`**: Contains observer emotional ratings sampled at 2Hz.
- **`target_rate_2Hz.csv`**: Contains target emotional ratings sampled at 2Hz.
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
        |-- rms.csv
        |-- flatness.csv
        |-- zerocross.csv
        |-- centroid.csv
        |-- rolloff.csv
```

## Error Handling for Missing Files

The scripts are designed to check for the existence of required files. If files are missing, an error message will prompt you to add them.

Example error handling in `setup.R`:

```r
if (!file.exists("observer_rate_2Hz.csv") || !file.exists("target_rate_2Hz.csv")) {
    stop("Error: observer_rate_2Hz.csv and/or target_rate_2Hz.csv are missing. Please add them to the main directory.")
}

if (!dir.exists("mir_features_240322")) {
    stop("Error: mir_features_240322 directory is missing. Please add it with necessary feature files.")
}
```

## Project Structure

Each script in the repository serves a specific purpose in the analysis pipeline. Below is an overview of the main scripts and their functions:

1. **`setup.R`**: Loads necessary libraries and sets up the working directory.

2. **`data_preparation.R`**: Prepares the data by importing and cleaning the emotional ratings and MIR features, and structures the dataset for analysis.

3. **`discrete_LME.R`**: Conducts Linear Mixed Effects modeling on discrete ratings (single-time emotional judgments) to identify predictors for different emotional states.

4. **`dynamic_LME_total.R`**: Compares LME models on dynamic ratings (continuous-time emotional judgments) to examine predictors of emotional changes.

5. **`dynamic_LME_joy_happiness.R`, `dynamic_LME_sadness.R`, `dynamic_LME_anger.R`**: Run dynamic LME analyses specifically focused on each individual emotion (joy, sadness, and anger).

6. **`discrete_cross_validation.R`**: Implements cross-validation on discrete LME models to validate the performance and generalizability of the model across different subsets of data.

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

3. View results in the console or in generated output files.

## License

This repository is released under the MIT License.