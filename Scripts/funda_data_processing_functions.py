import re
from datetime import datetime, timedelta
from typing import Union
from tqdm import tqdm
import googlemaps
import numpy as np
import pandas as pd
from dateutil.parser import parse
from config import google_api_key

gmaps = googlemaps.Client(key = google_api_key)

#################### Data Cleaning Functions ####################

def clean_price(x: str) -> int:
    """Clean the 'price' and transform from string to integer."""
    try:
        return int(str(x).split(" ")[1].replace(".", ""))
    except ValueError:
        return 0
    except IndexError:
        return 0


def clean_year(x: str) -> int:
    """Clean the 'year' and transform from string to integer."""
    x = x.strip().lower()  # Normalize the string to lowercase and remove leading/trailing spaces
    
    try:
        if x.isdigit() and len(x) == 4:
            return int(x)
        elif "-" in x:
            return int(x.split("-")[0])
        elif "voor" in x:
            year_part = x.split(" ")[-1]  # Use the last element after splitting to avoid IndexError
            return int(year_part) if year_part.isdigit() else 0  # Check if year_part is a digit
        elif "na" in x:
            year_part = x.split(" ")[-1]  # Use the last element after splitting
            # Assuming you want to return the year specified plus one; 
            # add error handling to avoid issues if year_part is not a digit
            return int(year_part) + 1 if year_part.isdigit() else 0
        else:
            return 0
    except ValueError:
        # Handle cases where conversion to int might fail, e.g., non-numeric strings
        return 0


def clean_living_area(x: str) -> int:
    """Clean the 'living_area' and transform from string to integer"""
    try:
        return int(str(x).replace(",", "").split(" m²")[0])
    except ValueError:
        return 0
    except IndexError:
        return 0


def find_keyword_from_regex(x: str, pattern: str) -> int:
    result = re.findall(pattern, x)
    if len(result) > 0:
        result = "".join(result[0])
        x = result.split(" ")[0]
    else:
        x = 0
    return int(x)


def find_n_room(x: str) -> int:
    """Find the number of rooms from a string"""
    pattern = r"(\d{1,2}\s{1}kamers{0,1})|(\d{1,2}\s{1}rooms{0,1})"
    return find_keyword_from_regex(x, pattern)


def find_n_bedroom(x: str) -> int:
    """Find the number of bedrooms from a string"""
    pattern = r"(\d{1,2}\s{1}slaapkamers{0,1})|(\d{1,2}\s{1}bedrooms{0,1})"
    return find_keyword_from_regex(x, pattern)


def find_n_bathroom(x: str) -> int:
    """Find the number of bathrooms from a string"""
    pattern = r"(\d{1,2}\s{1}badkamers{0,1})|(\d{1,2}\s{1}bathrooms{0,1})"
    return find_keyword_from_regex(x, pattern)


def map_dutch_month(x: str) -> str:
    """Map the month from Dutch to English."""
    month_mapping = {
        "januari": "January",
        "februari": "February",
        "maart": "March",
        "mei": "May",
        "juni": "June",
        "juli": "July",
        "augustus": "August",
        "oktober": "October",
    }
    for k, v in month_mapping.items():
        if x.find(k) != -1:
            x = x.replace(k, v)
    return x


def get_neighbor(x: str) -> str:
    """Find the neighborhood name."""
    city = x.split("/")[0].replace("-", " ")
    return x.lower().split(city)[-1]


def clean_energy_label(x: str) -> str:
    """Clean the energy labels."""
    try:
        x = x.split(" ")[0]
        if x.find("A+") != -1:
            x = ">A+"
        return x
    except IndexError:
        return x


def clean_list_date(x: str) -> Union[datetime, str]:
    """Transform the date from string to datetime object."""

    x = x.replace("weken", "week")
    x = x.replace("maanden", "month")
    x = x.replace("Vandaag", "Today")
    x = x.replace("+", "")
    x = map_dutch_month(x)

    def delta_now(d: int):
        t = timedelta(days=d)
        return datetime.now() - t

    weekdays_dict = {
        "maandag": "Monday",
        "dinsdag": "Tuesday",
        "woensdag": "Wednesday",
        "donderdag": "Thursday",
        "vrijdag": "Friday",
        "zaterdag": "Saturday",
        "zondag": "Sunday",
    }

    try:
        if x.lower() in weekdays_dict.keys():
            date_string = weekdays_dict.get(x.lower())
            parsed_date = parse(date_string, fuzzy=True)
            delta = datetime.now().weekday() - parsed_date.weekday()
            x = delta_now(delta)

        elif x.find("month") != -1:
            x = delta_now(int(x.split("month")[0].strip()[0]) * 30)
        elif x.find("week") != -1:
            x = delta_now(int(x.split("week")[0].strip()[0]) * 7)
        elif x.find("Today") != -1:
            x = delta_now(1)
        elif x.find("day") != -1:
            x = delta_now(int(x.split("day")[0].strip()))
        else:
            x = datetime.strptime(x, "%d %B %Y")
        return x

    except ValueError:
        return "na"

def preprocess_data(df: pd.DataFrame, is_past: bool) -> pd.DataFrame:
    """
    Clean the raw dataframe from scraping.
    Indicate whether the historical data is included since the columns would be different.

    :param df: raw dataframe from scraping
    :param is_past: whether it scraped past data
    :return: clean dataframe
    """

    # Processing steps that modify specific columns without dropping any
    #df["house_id"] = df["url"].apply(lambda x: int(x.split("/")[-2].split("-")[1])) # not really needed I think

    df['price_sold'] = df["price_sold"].apply(clean_price)
    df["living_area"] = df["living_area"].apply(clean_living_area)
    df["price_sold_m2"] = round(df.price_sold / df.living_area, 1)

    df["zip"] = df["zip_code"].apply(lambda x: x[:7])

    df["room"] = df["num_of_rooms"].apply(find_n_room)
    df["bedroom"] = df["num_of_rooms"].apply(find_n_bedroom)
    df["bathroom"] = df["num_of_bathrooms"].apply(find_n_bathroom)
    df["energy_label"] = df["energy_label"].apply(clean_energy_label)

    df["year_built"] = df["year"].apply(clean_year).astype(int)
    df["house_age"] = datetime.now().year - df["year_built"]

    df['date_sold'] = df['date_sold'].apply(clean_list_date)
    df['date_sold'] = pd.to_datetime(df['date_sold'], errors='coerce')

    df['date_listed'] = df['date_list'].apply(clean_list_date)
    df['date_listed'] = pd.to_datetime(df['date_listed'], errors='coerce')

    df["house_type"] = df["url"].apply(lambda x: x.split("/")[-2].split("-")[0])
    df = df[df["house_type"].isin(["appartement", "huis"])]

    df.loc[:, "full_address"] = df["address"] + ", " + df["zip"].astype(str) + ", " + df["city"]

    return df



# Geocoding the created full_address column --- full_address ---> latitude, longitude
def geocode_addresses(df, geocode_all=True):
    """
    Geocodes addresses in a DataFrame.
    
    Parameters:
    - df: DataFrame with a 'full_address' column and desired 'latitude' and 'longitude' columns.
    - geocode_all: If True, geocode all addresses. If False, only geocode addresses where
      latitude and longitude are not already populated.
    """
    
    # Ensure 'latitude' and 'longitude' columns exist
    if 'latitude' not in df.columns:
        df['latitude'] = np.nan
    if 'longitude' not in df.columns:
        df['longitude'] = np.nan
    
    # Iterate over DataFrame rows
    for index, row in tqdm(df.iterrows(), desc='Geocoding addresses', total=df.shape[0]):
        # Determine if we should geocode this row
        should_geocode = geocode_all or pd.isna(row['latitude']) or pd.isna(row['longitude'])
        
        if should_geocode and pd.notna(row['full_address']):
            try:
                geocode_result = gmaps.geocode(row['full_address'])
                if geocode_result:
                    location = geocode_result[0]['geometry']['location']
                    df.at[index, 'latitude'] = location['lat']
                    df.at[index, 'longitude'] = location['lng']
                else:
                    print(f"No results for address '{row['full_address']}'")
            except Exception as e:
                print(f"Error geocoding address '{row['full_address']}': {e}")
    
    # Ensure that the latitude and longitude columns are of float type
    df['latitude'] = df['latitude'].astype(float)
    df['longitude'] = df['longitude'].astype(float)
                
    return df

# Adding biking times to the { Central Business District (CBD) } or whatever other location
# Rotterda CBD coordinates
#destination_lat = 51.9244
#destination_lng = 4.477 

def add_biking_time(df, destination_lat = 51.9244, destination_lng = 4.477, calculate_for_missing_only=True):
    """
    Adds biking time to the DataFrame using Google Maps API, with an option to only calculate for missing values.
    
    Parameters:
    - df: DataFrame with 'latitude' and 'longitude' columns, default is Rotterdam CBD.
    - destination_lat: Latitude of the destination.
    - destination_lng: Longitude of the destination.
    - calculate_for_missing_only: If True, only calculate biking times for rows where the value is NA.
    """
    # Ensure there's a 'biking_time' column, initialize with np.nan if it doesn't exist
    if 'biking_time' not in df.columns:
        df['biking_time'] = np.nan
    
    # Iterate over DataFrame rows with progress tracking
    for index, row in tqdm(df.iterrows(), total=df.shape[0], desc='Calculating biking times'):
        # Determine if we should calculate biking time for this row
        should_calculate = not calculate_for_missing_only or pd.isna(row['biking_time'])
        
        if should_calculate and pd.notna(row['latitude']) and pd.notna(row['longitude']):
            origin = f"{row['latitude']},{row['longitude']}"
            destination = f"{destination_lat},{destination_lng}"
            
            try:
                directions_result = gmaps.directions(origin,
                                                     destination,
                                                     mode="bicycling",
                                                     departure_time=datetime.now())
                if directions_result:
                    # Extract travel time and convert to minutes
                    duration = directions_result[0]['legs'][0]['duration']['value'] / 60
                    df.at[index, 'biking_time'] = duration
            except Exception as e:
                print(f"Error retrieving biking time for index {index}: {e}")
    
    return df
