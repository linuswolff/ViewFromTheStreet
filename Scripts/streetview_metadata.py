from config import google_api_key
import requests
import pandas as pd
from datetime import datetime

def get_streetview_metadata(addresses, api_key, folder_name='streetview_metadata'):
    metadata_results = []
    for address in addresses:
        for heading in ['0', '90', '180', '270']:
            # Construct URL for the Street View metadata API
            url = f"https://maps.googleapis.com/maps/api/streetview/metadata?location={address}&size=320x320&heading={heading}&pitch=0&key={api_key}"
            
            # Make the API request
            response = requests.get(url)
            
            if response.status_code == 200:
                # Parse the JSON response
                metadata = response.json()
                
                # Process or save the metadata as needed
                metadata_results.append(metadata)
                print(metadata)  # For demonstration purposes; you might save this to a file instead
            else:
                print(f"Failed to fetch metadata for {address} at heading {heading}")

    # After collecting all metadata, you could write it to a file or process it as needed
    return metadata_results

# Example usage
addresses = ['Robert Baeldestraat 1, Rotterdam, Netherlands', 'Robert Baeldestraat 150, Rotterdam, Netherlands']
api_key = google_api_key

metadata = get_streetview_metadata(addresses, api_key)

metadata




# Import test.csv
funda_addresses = pd.read_csv('test.csv')

full_addresses = funda_addresses['address'] + ', ' + funda_addresses['zip'].astype(str) + ', ' + funda_addresses['city']


funda_metadata = get_streetview_metadata(full_addresses[:20], api_key)

# reveal the dim of funa_metadata

len(funda_metadata)

# Assuming funda_metadata is a list of dictionaries and each dictionary has a 'date' key
funda_dates = [entry['date'] for entry in funda_metadata if 'date' in entry]

# Assuming 'dates' is your list of date strings
filtered_dates = [date for date in funda_dates if 3 <= datetime.strptime(date, '%Y-%m').month <= 9]

print(filtered_dates)

len(filtered_dates)


meta = pd.read_json('funda_streetview_images_test/metadata.json')

# subset meta to only include dates between march and october
meta['date'] = pd.to_datetime(meta['date'])
meta_valid = meta[(meta['date'].dt.month >= 3) & (meta['date'].dt.month <= 9)]

meta_valid.shape

meta.shape