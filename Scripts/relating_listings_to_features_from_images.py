import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
from scipy.spatial import cKDTree
import numpy as np 

##########################################################################################
# Setup
##########################################################################################

# Load the data
print("Loading data...")
greenery = pd.read_csv('Data/greenery_index_results_big.csv')
obj_detect_counts = pd.read_csv('Data/object_detection_results_big.csv')
panoids = gpd.read_file('/Users/wolff/Desktop/delft_download/data/Rotterdam_NL/panoids/panoids.geojson')
listings = pd.read_csv('Data/funda_all_cleaned_may_23.csv')

# Calculate the average greenery index per panoid
print("Calculating average greenery index per panoid...")
greenery['Greenery_Index'] = greenery['Greenery_Index'].str.strip('[]').astype(float)
greenery_avg = greenery.groupby('panoid')['Greenery_Index'].mean().reset_index()

# Calculate the average object counts per panoid
print("Calculating average object counts per panoid...")
obj_detect_avg = obj_detect_counts.groupby('panoid').agg({
    'count_people': 'mean',
    'count_cars': 'mean',
    'count_bicycles': 'mean'
}).reset_index()

# Merge the average greenery index and object counts with the panoids dataframe
print("Merging average greenery index and object counts with panoids dataframe...")
panoids = panoids.merge(greenery_avg, on='panoid').merge(obj_detect_avg, on='panoid')

# Convert listings to GeoDataFrame
print("Converting listings to GeoDataFrame...")
listings['geometry'] = listings.apply(lambda row: Point(row['longitude'], row['latitude']), axis=1)
listings_gdf = gpd.GeoDataFrame(listings, geometry='geometry')

# Ensure the coordinate systems are projected to UTM for accurate distance calculations
print("Reprojecting coordinate systems to UTM...")
panoids = panoids.set_crs(epsg=4326).to_crs(epsg=32631)  # UTM zone 31N for Rotterdam, Netherlands
listings_gdf = listings_gdf.set_crs(epsg=4326).to_crs(epsg=32631)

##########################################################################################
# Function to calculate metrics
##########################################################################################

def add_metrics_to_listings(listings_gdf, panoids, radius):
    print(f"Calculating metrics for a {radius}m radius.")
    panoids_tree = cKDTree(np.vstack([panoids.geometry.x, panoids.geometry.y]).T)
    
    def calculate_metrics(listing):
        idx = panoids_tree.query_ball_point([listing.geometry.x, listing.geometry.y], radius)
        if idx:
            nearby_panoids = panoids.iloc[idx]
            avg_greenery = nearby_panoids['Greenery_Index'].mean()
            count_people = nearby_panoids['count_people'].mean()
            count_cars = nearby_panoids['count_cars'].mean()
            count_bicycles = nearby_panoids['count_bicycles'].mean()
            return avg_greenery, len(idx), count_people, count_cars, count_bicycles
        else:
            return np.nan, 0, np.nan, np.nan, np.nan
    
    results = listings_gdf.apply(calculate_metrics, axis=1, result_type='expand')
    listings_gdf[f'average_greenery_index_{radius}m'] = results[0]
    listings_gdf[f'assoc_panoid_count_{radius}m'] = results[1]
    listings_gdf[f'average_people_count_{radius}m'] = results[2]
    listings_gdf[f'average_cars_count_{radius}m'] = results[3]
    listings_gdf[f'average_bicycles_count_{radius}m'] = results[4]
    return listings_gdf

##########################################################################################
# Example usage
##########################################################################################

# Define the radius for metric calculation
radius = 250  # in meters

# Add metrics to the listings GeoDataFrame
listings_with_metrics = add_metrics_to_listings(listings_gdf, panoids, radius)

# Display the head of the second column and last 5 columns only
print("Displaying results...")
print(listings_with_metrics.iloc[:, [1] + list(range(-5, 0))].head())

##########################################################################################
# multiple radii
##########################################################################################

radii = [100, 250, 500, 1000]

# Add metrics to the listings GeoDataFrame
for radius in radii:
    listings_with_metrics = add_metrics_to_listings(listings_gdf, panoids, radius)  

# Display the head of the second column and last 5 columns only
print("Displaying results...")
print(listings_with_metrics.iloc[:, [1] + list(range(-5, 0))].head())

# Save the results to a CSV file
print("Saving results to CSV...")
listings_with_metrics.to_csv('Data/listings_may_23_with_features_added.csv', index=False)