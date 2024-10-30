import pandas as pd
import numpy as np
from scipy import interpolate


def interpolate_values(time, value, length, interval=0.5):
    """Interpolates given time and values using linear interpolation over a specified interval."""
    uni_time = np.arange(0, length + interval, interval)
    interp_func = interpolate.interp1d(time, value, kind="linear")
    interp_value = interp_func(uni_time)
    return uni_time.tolist(), interp_value.tolist()


def process_data(file_path, output_file, columns):
    """Processes data by reading the file, interpolating time and score values, and saving the result."""
    df = pd.read_csv(file_path, encoding="UTF-8")

    # Initialize new columns for the interpolated 2Hz data
    df["time_2Hz"] = np.nan
    df["score_2Hz"] = np.nan

    # Loop through each row to interpolate the 'time' and 'score' columns
    for i in range(len(df)):
        time = eval(
            df.loc[i, "time"]
        )  # Evaluate the 'time' column to convert string to list
        value = eval(
            df.loc[i, "score"]
        )  # Evaluate the 'score' column to convert string to list
        length = int(df.loc[i, "length"])

        # Interpolate time and score at 2Hz (every 0.5 seconds)
        time_2Hz, score_2Hz = interpolate_values(time, value, length)

        # Store the interpolated values back into the dataframe
        df.at[i, "time_2Hz"] = str(time_2Hz)
        df.at[i, "score_2Hz"] = str(score_2Hz)

    # Select relevant columns and save the new CSV file
    df = df[columns]
    df.to_csv(output_file, index=False)


# 1. Processing for 'target' dataset
# target: composers' emotional intention ratings while watching their own recorded videos
process_data(
    file_path="target_rate.csv",
    output_file="target_rate_2Hz.csv",
    columns=[
        "id",
        "type",
        "emotion",
        "length",
        "valence",
        "arousal",
        "dominance",
        "fss",
        "time_2Hz",
        "score_2Hz",
        "time",
        "score",
    ],
)

# 2. Processing for 'observer' dataset
# observer: music listeners who listened to music in audio-only
process_data(
    file_path="observer_videorate.csv",
    output_file="observer_rate_2Hz.csv",
    columns=[
        "id",
        "target",
        "emotion",
        "length",
        "valence",
        "arousal",
        "dominance",
        "flow",
        "empathy",
        "time_2Hz",
        "score_2Hz",
        "time",
        "score",
    ],
)
