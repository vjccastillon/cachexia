#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 20:15:28 2024

@author: castilv
"""
import os
import sys

# Set the working directory to where your scripts are
script_directory = '/Users/castilv/Documents/Cachexia/cac_data/scripts/cac_identification'
os.chdir(script_directory)

# Ensure the directory is added to Python's search path
sys.path.append(script_directory)

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
from tqdm import tqdm

from load_data import load_and_process_bmi_data, smooth_bmi_ewma
from cachexia_identification import identify_cachexia_episodes, identify_recovery_episodes, merge_episodes

bmi_data_path = '/Users/castilv/Documents/Cachexia/cac_data/bmi_upd.csv'
metadata_path = '/Users/castilv/Documents/Cachexia/cac_data/clean_metadata/metadata_clin_0930.csv'
output_path = '/Users/castilv/Documents/Cachexia/cac_data/output/'

def main():
    bmi_data = load_and_process_bmi_data(bmi_data_path, metadata_path)
    bmi_data = smooth_bmi_ewma(bmi_data, 'BMI', 0.2)
    
    episodes = identify_cachexia_episodes(bmi_data, 'Days_Since_Diagnosis', 'smoothed_BMI')
    episodes = merge_episodes(episodes, 'start_day', 'end_day')
    episodes = identify_recovery_episodes(bmi_data, episodes, 'Days_Since_Diagnosis', 'smoothed_BMI')
    
    episodes.to_csv(output_path + 'processed_episodes.csv', index=False)
    print("Analysis complete. Processed episodes saved to:", output_path + 'processed_episodes.csv')

if __name__ == "__main__":
    main()
