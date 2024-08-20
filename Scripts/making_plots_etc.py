import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Load the data
listings_all = pd.read_csv('./Data/listings_may_23_with_features_added.csv')

# Create a GeoDataFrame
gdf = gpd.GeoDataFrame(listings_all, geometry=gpd.points_from_xy(listings_all.longitude, listings_all.latitude))

# Set the coordinate reference system to WGS84
gdf.crs = 'EPSG:4326'

# Plotting
fig, ax = plt.subplots(1, 1, figsize=(12, 8))

# Define colors for each house type
colors = {'huis': 'blue', 'appartement': 'green'}

# Plot each house type with a different color
for house_type, color in colors.items():
    subset = gdf[gdf.house_type == house_type]
    subset.plot(ax=ax, marker='o', color=color, markersize=50, label=house_type, alpha=0.6)

# Add a legend
plt.legend(title='House Type')

# Add a title
plt.title('House Listings by Type')

# Show the plot
plt.show()



####


import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from pathlib import Path

# Load the data
data_path = Path('/Users/wolff/Desktop/delft_download/data/Rotterdam_NL')
panoids = gpd.read_file(data_path / 'panoids/panoids.geojson')
greenery_data = pd.read_csv('./Data/greenery_index_results_big.csv')

# Clean the Greenery_Index column
greenery_data['Greenery_Index'] = greenery_data['Greenery_Index'].str.strip('[]').astype(float)

# Aggregate by the panoid column, not image name (mean)
greenery_data = greenery_data.groupby('panoid').agg({'Greenery_Index': 'mean'}).reset_index()

# Merge the datasets on the panoid column
df = pd.merge(panoids[['panoid', 'lat', 'lng']], greenery_data, on='panoid', how='inner')

# Reorder columns if needed
df = df[['panoid', 'Greenery_Index', 'lat', 'lng']]

# Create a GeoDataFrame
gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lng, df.lat))

# Create the plot
fig, ax = plt.subplots(figsize=(12, 8))

map_center = [df['lat'].mean(), df['lng'].mean()]

# Create a GeoDataFrame
gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lng, df.lat), crs="EPSG:4326")

# Create the plot
fig, ax = plt.subplots(figsize=(12, 8))

# Plot the points
scatter = ax.scatter(gdf.geometry.x, gdf.geometry.y, c=gdf.Greenery_Index, 
                     cmap='YlGn', s=10, alpha=0.5)

# Customize the plot
ax.set_title('Greenery Index in Rotterdam')
ax.set_xlabel('Longitude')
ax.set_ylabel('Latitude')
plt.colorbar(scatter, label='Greenery Index')

# Set the extent of the map to cover all points
ax.set_xlim(gdf.total_bounds[[0, 2]])
ax.set_ylim(gdf.total_bounds[[1, 3]])

# Save the figure
plt.savefig('rotterdam_greenery_map.png', dpi=300, bbox_inches='tight')

# Display the plot
plt.show()


import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from PIL import Image
import os

# Define the file paths
base_path = "/Users/wolff/Desktop/street_view_images/"
valid_images = ["image_167114_b.png", "image_188983_s_a.png", "image_153394_b.png"]
invalid_images = ["image_54216_s_b.png", "image_6194_f.png", "image_54273_f.png"]

# Combine all image paths
all_images = valid_images + invalid_images

# Set up the plot
fig, axes = plt.subplots(2, 3, figsize=(8, 6))

# Plot each image in the grid
for ax, img_name in zip(axes.flatten(), all_images):
    img_path = os.path.join(base_path, img_name)
    img = mpimg.imread(img_path)
    ax.imshow(img)
    ax.axis('off')  # Hide axes

# Add titles for valid and invalid sections
for ax, label in zip(axes[0], ["Valid"] * 3):
    ax.set_title(label, fontsize=12)
for ax, label in zip(axes[1], ["Invalid"] * 3):
    ax.set_title(label, fontsize=12)

# Adjust layout
plt.tight_layout()
plt.show()


from arcgis.gis import GIS
from arcgis.features import FeatureLayer
import geopandas as gpd

# Connect to ArcGIS online
gis = GIS()

# Specify the URL of the Feature Layer
url = "https://services.arcgis.com/zP1tGdLpGvt2qNJ6/ArcGIS/rest/services/wijkraden_Rotterdam/FeatureServer/0"

# Create a FeatureLayer object
layer = FeatureLayer(url)

# Query all features from the layer
features = layer.query(where="1=1", out_fields="*", return_geometry=True)

geojson = features.to_geojson

import json
# If you want to save the GeoJSON to a file:
with open('output.geojson', 'w') as f:
    json.dump(geojson, f)


# Convert to a GeoDataFrame and set the CRS
rotterdam_areas = features.sdf
