# config.py
### This files is used to load api keys and other config stuff


### Google API Key
from dotenv import load_dotenv
import os

load_dotenv()

google_api_key = os.getenv('google_api_key')