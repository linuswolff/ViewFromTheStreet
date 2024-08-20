import google_streetview.api
import os

def get_streetview_images(address, api_key):
    # Prepare a sanitized version of the address to use in filenames (remove illegal characters, etc.)
    safe_address = address.replace(' ', '_').replace(',', '').replace('/', '_')
    headings = ['0', '90', '180', '270']
    
    for heading in headings:
        params = [{
            'size': '320x320',
            'location': address,
            'key': api_key,
            'heading': heading,
            'pitch': '0',
            'fov': '60',
            'source': 'outdoor'
        }]
        
        # Create a results object for each heading
        results = google_streetview.api.results(params)
        # Download images to a common directory
        results.download_links('streetview_images')
        
        # Assuming the API saves files with a predictable name or there's a way to retrieve the filename
        # You might need to adjust this logic based on how files are actually named/saved
        # This is a placeholder loop to illustrate renaming logic
        for file in os.listdir('streetview_images'):
            # Construct the new filename using the address and heading
            new_filename = f"{safe_address}_heading_{heading}.jpg"
            old_file_path = os.path.join('streetview_images', file)
            new_file_path = os.path.join('streetview_images', new_filename)
            
            # Rename the file
            os.rename(old_file_path, new_file_path)

# Usage example
address = 'Robert Baeldestraat 111, 3061TH Rotterdam, Netherlands'
api_key = 'xxx'

get_streetview_images(address, api_key)
