"""
Script to unify the three dayrange dataframes from arrhenius_stats.R

Author: Serena G. Lotreck
"""
import argparse
from os.path import abspath
from functools import reduce

import pandas as pd


def main(dfs, outpath):
    """
    Merges dfs in a list over common columns.

    parameters:
        dfs, list of pandas df: dfs to merge
        outpath, str: Path to save, including name

    returns: None
    """
    # Merge dfs
    print('\nMerging dataframes...')
    df_final = reduce(lambda df1, df2: pd.merge(df1, df2, how='outer'), dfs)

    # Assert that they all have the same number of rows as the merged result
    print('\nAsserting that no rows have been dropped...')
    for df in dfs:
        assert df.shape[0] == df_final.shape[0], (
            "Rows have been dropped or added!")

    # Save out result
    print('\nSaving out result...')
    df_final.to_csv(outpath, index=False)

    print('\nDone!')


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Unify three dataframes")

    parser.add_argument('dfs', nargs='+',
            help='Paths to dfs to join')
    parser.add_argument('outpath', type=str,
            help='Path to save output,  including file name')

    args = parser.parse_args()

    df_paths = [abspath(df_path) for df_path in args.dfs]
    dfs = [pd.read_csv(df_path) for df_path in df_paths]

    args.outpath = abspath(args.outpath)

    main(dfs, args.outpath)
