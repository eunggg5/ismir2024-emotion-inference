import pandas as pd
import numpy as np
from scipy import interpolate

mir = ['mir_features_240322/mir_rms.csv', 'mir_features_240322/mir_centroid.csv', 
       'mir_features_240322/mir_zerocross.csv']


for feature in mir:
    df = pd.read_csv(feature, encoding='UTF-8', header=None)

    # print(df.shape[1])

    # print(df.iloc[0, 0:5000])
    # print(np.nonzero(df.iloc[0, 0:5000]))
    # print(np.max(np.nonzero(df.iloc[0, 0:5000])))
    # print(df.iloc[0, 3895])
    # print(df.iloc[0, 1:3895].tolist())

    for i in range(1, len(df)):

        value = eval(df.loc[i, 1])
        # print(value)
        # print('%%%%%%%%%%%%%%%%%%%%%%%%')
        length = len(value) // 2
        print(length)

        # uni_time = np.arange(0, time, 0.5)
        uni_time = np.arange(0, length, 1)
        interp_func = interpolate.interp1d(np.arange(len(value)), value, kind='linear')
        interp_value = interp_func(uni_time)

        time_1s = uni_time.tolist()
        value_1s = interp_value.tolist()

        if (df.loc[i, 0] == "eat01_anger.wav"):
            time_1s = time_1s[4:len(time_1s)-3]
            value_1s = value_1s[4:len(value_1s)-3]
        elif (df.loc[i, 0] == "eat01_joy.wav"):
            time_1s = time_1s[1:len(time_1s)-4]
            value_1s = value_1s[1:len(value_1s)-4]
        elif (df.loc[i, 0] == "eat02_anger.wav"):
            time_1s = time_1s[3:len(time_1s)-3]
            value_1s = value_1s[3:len(value_1s)-3]
        elif (df.loc[i, 0] == "eat02_joy.wav"):
            time_1s = time_1s[2:len(time_1s)-3]
            value_1s = value_1s[2:len(value_1s)-3]
        elif (df.loc[i, 0] == "eat02_sad.wav"):
            time_1s = time_1s[4:len(time_1s)-2]
            value_1s = value_1s[4:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat06_joy.wav"):
            time_1s = time_1s[1:len(time_1s)-2]
            value_1s = value_1s[1:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat06_sad.wav"):
            time_1s = time_1s[4:len(time_1s)-1]
            value_1s = value_1s[4:len(value_1s)-1]
        elif (df.loc[i, 0] == "eat09_anger.wav"):
            time_1s = time_1s[3:len(time_1s)]
            value_1s = value_1s[3:len(value_1s)]
        elif (df.loc[i, 0] == "eat09_joy.wav"):
            time_1s = time_1s[3:len(time_1s)-2]
            value_1s = value_1s[3:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat09_sad.wav"):
            time_1s = time_1s[2:len(time_1s)-2]
            value_1s = value_1s[2:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat14_anger.wav"):
            time_1s = time_1s[3:len(time_1s)-2]
            value_1s = value_1s[3:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat14_sad.wav"):
            time_1s = time_1s[5:len(time_1s)-3]
            value_1s = value_1s[5:len(value_1s)-3]
        elif (df.loc[i, 0] == "eat16_anger.wav"):
            time_1s = time_1s[3:len(time_1s)-7]
            value_1s = value_1s[3:len(value_1s)-7]
        elif (df.loc[i, 0] == "eat16_joy.wav"):
            time_1s = time_1s[3:len(time_1s)-4]
            value_1s = value_1s[3:len(value_1s)-4]
        elif (df.loc[i, 0] == "eat16_sad.wav"):
            time_1s = time_1s[5:len(time_1s)-4]
            value_1s = value_1s[5:len(value_1s)-4]
        elif (df.loc[i, 0] == "eat17_anger.wav"):
            time_1s = time_1s[4:len(time_1s)-1]
            value_1s = value_1s[4:len(value_1s)-1]
        elif (df.loc[i, 0] == "eat17_joy.wav"):
            time_1s = time_1s[3:len(time_1s)-2]
            value_1s = value_1s[3:len(value_1s)-2]
        elif (df.loc[i, 0] == "eat17_sad.wav"):
            time_1s = time_1s[6:len(time_1s)-2]
            value_1s = value_1s[6:len(value_1s)-2]

        nor_value = (value_1s - np.min(value_1s)) / (np.max(value_1s) - np.min(value_1s))

        # df.loc[i, 'time_2Hz'] = str(time_2Hz)
        # df.loc[i, feature[20:len(feature)-13]] = str(value_2Hz)

        median = np.median(nor_value)
        mean = np.mean(nor_value)
        sd = np.std(nor_value)

        df.loc[i, 'median'] = median
        df.loc[i, 'mean'] = mean
        df.loc[i, 'sd'] = sd

        df.loc[i, 'nor_value'] = str(nor_value)
        df.loc[i, 'time_1s'] = str(time_1s)
        df.loc[i, 'score_1s'] = str(value_1s)

    # print(df.iloc[:, [0, 5000, 5001]])

    print(df)

    df.to_csv(feature + '_1s.csv')
    