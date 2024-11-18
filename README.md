# cachexia
Episode Identification - BMI Trajectory

This project automates the identification of cachexia episodes using BMI data trajectories. It processes patient BMI data, applies smoothing techniques, and identifies episodes of significant BMI decrease, which are indicative of cachexia.

## Features
load_data.py - Data Processing: Clean and prepare BMI data
  1: **load_and_process_bmi_data(bmi_path, metadata_path)** - 
    *Standardize data by days_since diagnosis
    *Filtering out BMI outliers BMI<10 and BMI>70
    *Exclude patients with trajectories less than 180 days
  2: **smooth_bmi_ewma(df, smooth_col, alpha)** -
    *Smooth data using EMWA (alpha = smoothing factor)
cachexia_identification.py- Episode Identification: Detect cachexia episodes based on a defined threshold of BMI loss over time.
  1: **identify_cachexia_episodes(df, time_col, bmi_col, recovery=True)** - sliding window to identify cachectic episodes 
    * Identifies start, onset, end of each episode
  2: **identify_recovery_episodes(patient_data, merged_episodes_df, time_col, bmi_col)** - identifies potential recovery 
    * Identifies 5% increase recovery if applicable
  3: **merge_episodes(df, start_col, end_col)** - merge overlapping episodes

  
