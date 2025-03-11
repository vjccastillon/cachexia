#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 21 20:21:22 2024

@author: castilv
"""


import pandas as pd

#episode_spans=pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/msk_spans_lab_0729.csv')
episode_spans=pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/processed_episodes/data/processed_data/cachexia_episodes_all_patients_20241111.csv')
msk_clin= pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/metadata_clin_0930.csv')
bmi= pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/bmi_clin.csv')


bmi['datetime'] = pd.to_datetime(bmi['datetime'])
bmi_sorted = bmi.sort_values(by=['MRN', 'datetime'])
first_bmi = bmi_sorted.drop_duplicates(subset=['MRN'], keep='first')[['MRN', 'datetime']]

first_bmi = first_bmi.rename(columns={'datetime': 'first_bmi_date'})

msk_clin_subset = msk_clin[['MRN', 'Tumor Diagnosis Date', 'PLA_LAST_CONTACT_DTE']]
episode_spans = pd.merge(episode_spans, msk_clin_subset, on='MRN', how='left')
episode_spans = pd.merge(episode_spans, first_bmi, on='MRN', how='left')

episode_spans['Tumor Diagnosis Date'] = pd.to_datetime(episode_spans['Tumor Diagnosis Date'], errors='coerce')
episode_spans['first_bmi_date'] = pd.to_datetime(episode_spans['first_bmi_date'], errors='coerce')
episode_spans['PLA_LAST_CONTACT_DTE'] = pd.to_datetime(episode_spans['PLA_LAST_CONTACT_DTE'], errors='coerce')
episode_spans['start_date'] = pd.to_datetime(episode_spans['start_date'], errors='coerce')
episode_spans['end_date'] = pd.to_datetime(episode_spans['end_date'], errors='coerce')

episode_spans['days_since_diagnosis'] = 0 
episode_spans['first_bmi_days'] = (episode_spans['first_bmi_date'] - episode_spans['Tumor Diagnosis Date']).dt.days
episode_spans['last_contact_days'] = (episode_spans['PLA_LAST_CONTACT_DTE'] - episode_spans['Tumor Diagnosis Date']).dt.days

#episode_spans['start_day'] = pd.to_numeric(episode_spans['start_day'], errors='coerce')
episode_spans['no_episodes'] = episode_spans.groupby('MRN')['start_day'].transform(lambda x: x.notna().sum())



episode_spans = episode_spans[[
    'MRN', 
    'days_since_diagnosis', 
    'first_bmi_days',
    'start_day',  
    'end_day',
    'last_contact_days',
    'no_episodes'
]]

episode_spans['span'] = episode_spans['no_episodes'].apply(lambda x: 0 if x == 0 else 1)


def calculate_spans(df):
    spans = []

    for mrn, group in df.groupby('MRN'):
        group.sort_values('start_day', inplace=True)
        
      
        first_bmi_day = group['first_bmi_days'].min()
        diagnosis_day = group['days_since_diagnosis'].min()
        last_contact_day = group['last_contact_days'].max()
        
      
        start_of_monitoring = min(first_bmi_day, diagnosis_day)
        last_processed_day = start_of_monitoring

        if group['no_episodes'].iloc[0] == 0:
         
            spans.append({
                'MRN': mrn,
                'start_day': start_of_monitoring,
                'end_day': last_contact_day,
                'span': 0
            })
        else:
         
            processed_cachectic_span = False
            for index, row in group.iterrows():
                start_day = row['start_day']
                end_day = row['end_day']
                
             
                if last_processed_day < start_day:
                    spans.append({
                        'MRN': mrn,
                        'start_day': last_processed_day,
                        'end_day': start_day - 1,
                        'span': 0
                    })

               
                spans.append({
                    'MRN': mrn,
                    'start_day': start_day,
                    'end_day': end_day,
                    'span': 1
                })

                last_processed_day = end_day + 1
                processed_cachectic_span = True

           
            if last_processed_day <= last_contact_day:
                spans.append({
                    'MRN': mrn,
                    'start_day': last_processed_day,
                    'end_day': last_contact_day,
                    'span': 0
                })

    spans_df = pd.DataFrame(spans)
    return spans_df

calculated_spans = calculate_spans(episode_spans)
print(calculated_spans.head(20))

calculated_spans.to_csv('/Users/castilv/Documents/Cachexia/cac_data/spans_episodes_0224.csv', index=False)


import pandas as pd

episode_spans=pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/processed_episodes/data/processed_data/cachexia_episodes_all_patients_20241111.csv')
msk_clin= pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/metadata_clin_0930.csv')

bmi= pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/bmi_clin.csv')


bmi['datetime'] = pd.to_datetime(bmi['datetime'])
bmi_sorted = bmi.sort_values(by=['MRN', 'datetime'])
first_bmi = bmi_sorted.drop_duplicates(subset=['MRN'], keep='first')[['MRN', 'datetime']]

first_bmi = first_bmi.rename(columns={'datetime': 'first_bmi_date'})

msk_clin_subset = msk_clin[['MRN', 'Tumor Diagnosis Date', 'PLA_LAST_CONTACT_DTE']]
episode_spans = pd.merge(episode_spans, msk_clin_subset, on='MRN', how='left')
episode_spans = pd.merge(episode_spans, first_bmi, on='MRN', how='left')

episode_spans['Tumor Diagnosis Date'] = pd.to_datetime(episode_spans['Tumor Diagnosis Date'], errors='coerce')
episode_spans['first_bmi_date'] = pd.to_datetime(episode_spans['first_bmi_date'], errors='coerce')
episode_spans['PLA_LAST_CONTACT_DTE'] = pd.to_datetime(episode_spans['PLA_LAST_CONTACT_DTE'], errors='coerce')
episode_spans['start_date'] = pd.to_datetime(episode_spans['start_date'], errors='coerce')
episode_spans['end_date'] = pd.to_datetime(episode_spans['end_date'], errors='coerce')

episode_spans['days_since_diagnosis'] = 0 
episode_spans['first_bmi_days'] = (episode_spans['first_bmi_date'] - episode_spans['Tumor Diagnosis Date']).dt.days
episode_spans['last_contact_days'] = (episode_spans['PLA_LAST_CONTACT_DTE'] - episode_spans['Tumor Diagnosis Date']).dt.days

episode_spans['no_episodes'] = episode_spans.groupby('MRN')['start_day'].transform(lambda x: x.notna().sum())

episode_spans['span'] = episode_spans['no_episodes'].apply(lambda x: 0 if x == 0 else 1)
episode_spans = episode_spans[[
    'MRN', 
    'days_since_diagnosis', 
    'first_bmi_days',
    'start_day',  
    'end_day',
    'last_contact_days',
    'no_episodes',
    'recovery_day'  # Including recovery_day for the recovery span check
]]


# Define function to calculate spans, now including recovery classification
def calculate_spans(df):
    spans = []  # List to store each span dictionary

    # Group by MRN to handle each patient individually
    for mrn, group in df.groupby('MRN'):
        group.sort_values('start_day', inplace=True)
        
        # Get first BMI day, diagnosis day, and last contact day for each patient
        first_bmi_day = group['first_bmi_days'].min()
        diagnosis_day = group['days_since_diagnosis'].min()
        last_contact_day = group['last_contact_days'].max()
        
        # Determine the start of monitoring (earliest of BMI or diagnosis date)
        start_of_monitoring = min(first_bmi_day, diagnosis_day)
        last_processed_day = start_of_monitoring  # Track the last day processed

        # If no cachectic episodes, create a single non-cachectic span
        if group['no_episodes'].iloc[0] == 0:
            spans.append({
                'MRN': mrn,
                'start_day': start_of_monitoring,
                'end_day': last_contact_day,
                'span': 0  # Non-cachectic span
            })
        else:
            # Process each row representing cachectic episodes
            for index, row in group.iterrows():
                start_day = row['start_day']
                end_day = row['end_day']
                recovery_day = row['recovery_day'] if pd.notna(row['recovery_day']) else None

                # Add a non-cachectic span if there is a gap before the current cachectic episode
                if last_processed_day < start_day:
                    spans.append({
                        'MRN': mrn,
                        'start_day': last_processed_day,
                        'end_day': start_day - 1,
                        'span': 0  # Non-cachectic span
                    })

                # Add the cachectic span for the current episode
                spans.append({
                    'MRN': mrn,
                    'start_day': start_day,
                    'end_day': end_day,
                    'span': 1  # Cachectic span
                })

                last_processed_day = end_day + 1  
                
                if recovery_day and recovery_day > end_day:
                    spans.append({
                        'MRN': mrn,
                        'start_day': end_day + 1,
                        'end_day': recovery_day,
                        'span': 2  # Recovery span
                    })
                    last_processed_day = recovery_day + 1 

            if last_processed_day <= last_contact_day:
                spans.append({
                    'MRN': mrn,
                    'start_day': last_processed_day,
                    'end_day': last_contact_day,
                    'span': 0  
                })

    # Convert list of spans to DataFrame
    spans_df = pd.DataFrame(spans)
    return spans_df

# Calculate spans with the updated function
calculated_spans = calculate_spans(episode_spans)
print(calculated_spans.head(20))

# Save the calculated spans to a CSV file
calculated_spans.to_csv('/Users/castilv/Documents/Cachexia/cac_data/spans_episodes_with_recovery.csv', index=False)
