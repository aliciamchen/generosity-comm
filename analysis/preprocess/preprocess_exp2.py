import argparse
import glob
import json
import os

import numpy as np
import pandas as pd

def main(args):

    in_dir = os.path.join(in_base_dir, str(args.in_dir))
    out_dir = os.path.join(out_base_dir, str(args.out_dir))
    expt_label = str(args.expt_label)

    if not os.path.exists(out_dir):
        os.mkdir(out_dir)

    # Clean data, make csv files

    filenames = sorted(glob.glob(in_dir + "/*responses.json"))

    dfs_demographics = []
    dfs_data = []

    for filename in filenames:

        with open(filename) as f:
            raw_json_data = json.load(f)

        demographics = raw_json_data[-1]
        data = raw_json_data[:-2]
        attention = raw_json_data[-2]

        df_demographics = pd.DataFrame(data={
            'subject_id': demographics['subject_id'],
            'time_elapsed': demographics['time_elapsed'],
            'gender': demographics['response']['gender'],
            'age': int(demographics['response']['age']),
            'understood': demographics['response']['understood'],
            'pass_attention': attention['passAttentionCheck'],
            'comments': demographics['response']['comments'],
        }, index=[0])

        # Make data dataframe
        cols = ['subject_id', 'story', 'social_interaction', 'more', 'equal', 'less']
        df_data = pd.DataFrame(columns=cols)

        for i, trial in enumerate(data):
            df_data.loc[i, 'subject_id'] = trial['subject_id']
            df_data.loc[i, 'story'] = trial['story']
            df_data.loc[i, 'social_interaction'] = trial['social_interaction']
            df_data.loc[i, 'more'] = trial['response']['more'] # spelling error lol
            df_data.loc[i, 'equal'] = trial['response']['equal']
            df_data.loc[i, 'less'] = trial['response']['less']

        # Add bonus info and understood instructions (for exclusion criteria)
        df_data['understood'] = demographics['response']['understood']
        df_data['pass_attention'] = attention['passAttentionCheck']

        dfs_demographics.append(df_demographics)
        dfs_data.append(df_data)

    df_demographics_all = pd.concat(dfs_demographics, ignore_index=True)
    df_data_all = pd.concat(dfs_data, ignore_index=True)

    df_demographics_all.to_csv(os.path.join(
        out_dir, f"{expt_label}_demographics.csv"), index=False)
    df_data_all.to_csv(os.path.join(out_dir, f"{expt_label}_data.csv"), index=False)


if __name__ == "__main__":

    in_base_dir = "../../"
    out_base_dir = "../../"

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--in_dir",
        required=True,
        help="raw data directory (relative to project directory)"
    )

    parser.add_argument(
        "--out_dir",
        required=True,
        help="output directory (relative to project directory)"
    )

    parser.add_argument(
        "--expt_label",
        required=True
    )

    args = parser.parse_args()

    main(args)
