# cachexia
Episode Identification - BMI Trajectory

This project automates the identification of cachexia episodes using BMI data trajectories. It processes patient BMI data, applies smoothing techniques, and identifies episodes of significant BMI decrease, which are indicative of cachexia.

## Features

### `load_data.py` - Data Processing
Clean and prepare BMI data through various functions:

- **`load_and_process_bmi_data(bmi_path, metadata_path)`**:
  - Standardizes data by days since diagnosis.
  - Filters out BMI outliers (BMI < 10 and BMI > 70).
  - Excludes patients with trajectories less than 180 days.

- **`smooth_bmi_ewma(df, smooth_col, alpha)`**:
  - Smooths data using Exponentially Weighted Moving Average (EWMA) where `alpha` is the smoothing factor.

### `cachexia_identification.py` - Episode Identification
Detect cachexia episodes based on a defined threshold of BMI loss over time:

- **`identify_cachexia_episodes(df, time_col, bmi_col, recovery=True)`**:
  - Uses a sliding window to identify cachectic episodes.
  - Identifies start, onset, and end of each episode.

- **`identify_recovery_episodes(patient_data, merged_episodes_df, time_col, bmi_col)`**:
  - Identifies potential recoveries with a 5% increase in BMI if applicable.

- **`merge_episodes(df, start_col, end_col)`**:
  - Merges overlapping episodes to streamline episode data.

  
