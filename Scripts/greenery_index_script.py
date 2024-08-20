import torch
from PIL import Image
from transformers import AutoImageProcessor, Mask2FormerForUniversalSegmentation
import numpy as np
import geopandas as gpd
from pathlib import Path
import pandas as pd
from tqdm import tqdm
from torch.utils.data import DataLoader, Dataset

# Check if MPS is available and set the device
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
print(f"Using device: {device}")

# Load Mask2Former fine-tuned on Cityscapes semantic segmentation
processor = AutoImageProcessor.from_pretrained("facebook/mask2former-swin-small-cityscapes-semantic")
model = Mask2FormerForUniversalSegmentation.from_pretrained("facebook/mask2former-swin-small-cityscapes-semantic")
model.to(device)
print(f"Model {model.config.model_type} loaded successfully.")

# Define paths and load data
data_path = Path('/Users/wolff/Desktop/delft_download/data/Rotterdam_NL')
panoids = gpd.read_file(data_path / 'panoids/panoids.geojson')
print(f"Loaded panoids data with {len(panoids)} entries.")


# dimensions of the panoids
#panoids.shape

### ============================================
### Filter Panoids: May to September, from 2017
### ============================================

# Apply the filtering criteria: Only use panoids between June and September, starting from the year 2017
panoids = panoids[
    (panoids['year'] >= 2017) & 
    (panoids['month'] >= 6) & 
    (panoids['month'] <= 9)
].reset_index(drop=True)

# panoids_test.shape

print(f"Filtered panoids data with {len(panoids)} entries remaining.")

### ============================================

# Function to get image paths for a given panoid
def get_image_paths(panoid):
    paths = panoids[panoids['panoid'] == panoid][['im_side_a', 'im_front', 'im_side_b', 'im_back']]
    paths = paths.values.tolist()[0]
    def img_path(x):
        return Path("/Users/wolff/Desktop/python_copy_2") / f"{x}"
    return [img_path(x) for x in paths]

# Custom dataset class for loading images
class PanoDataset(Dataset):
    def __init__(self, panoids):
        self.panoids = panoids

    def __len__(self):
        return len(self.panoids)

    def __getitem__(self, idx):
        panoid = self.panoids.iloc[idx]['panoid']
        image_paths = get_image_paths(panoid)
        images = []
        for image_path in image_paths:
            if image_path.exists():
                images.append(processor(images=Image.open(image_path).convert("RGB"), return_tensors="pt")["pixel_values"].squeeze(0))
            else:
                print(f"Warning: File not found {image_path}. Skipping this image.")
        if not images:
            print(f"Skipping panoid {panoid}. All images are missing.")
            return None  # Return None to indicate that this entry should be skipped
        return panoid, image_paths, images

# Custom collate function to handle PosixPath and tensor batching
def custom_collate_fn(batch):
    # Filter out None entries
    batch = [item for item in batch if item is not None]
    if not batch:
        return [], [], []  # Return empty lists if the batch is empty
    panoids, image_paths_batch, images_batch = zip(*batch)
    return list(panoids), list(image_paths_batch), [torch.stack(images) for images in images_batch]

# Function to calculate the Greenery Index for a batch of images
def calculate_greenery_index_batch(images):
    inputs = torch.stack(images).to(device)
    
    with torch.no_grad():
        outputs = model(pixel_values=inputs)

    # Post-process the outputs to get the semantic segmentation maps
    predicted_semantic_maps = processor.post_process_semantic_segmentation(outputs, target_sizes=[(image.shape[1], image.shape[2]) for image in images])

    greenery_indices = []
    for predicted_semantic_map in predicted_semantic_maps:
        # Convert the entire map to a 1D array of labels
        predicted_labels = predicted_semantic_map.cpu().numpy().flatten()
        unique_labels, counts = np.unique(predicted_labels, return_counts=True)
        
        # Calculate total number of pixels
        total_pixels = predicted_labels.size
        
        # Identify indicers for 'vegetation' and 'terain'
        vegetation_indices = [idx for idx, label in model.config.id2label.items() if label.lower() == 'vegetation']
        terrain_indices = [idx for idx, label in model.config.id2label.items() if label.lower() == 'terrain']
        
        # Calculate the number of vegetation and terrain pixels
        greenery_pixels = sum(counts[unique_labels == idx] for idx in vegetation_indices + terrain_indices if idx in unique_labels)
        
        # Calculate Greenery Index
        greenery_index = greenery_pixels / total_pixels
        greenery_indices.append(greenery_index)

    return greenery_indices

# Function to read existing results and determine already processed panoids
def read_existing_results(csv_path):
    if not csv_path.exists():
        return set()
    df = pd.read_csv(csv_path)
    unique_panoids = set(df['panoid'].unique())
    print(f"Read {len(unique_panoids)} unique panoids from existing results.")
    return unique_panoids

# Function to process a chunk of panoids
def process_chunk(panoids_chunk, results_file):
    pano_dataset = PanoDataset(panoids_chunk)
    data_loader = DataLoader(pano_dataset, batch_size=1, shuffle=False, collate_fn=custom_collate_fn)
    
    chunk_results = []

    for panoid_batch, image_paths_batch, images_batch in tqdm(data_loader, desc="Processing Pano IDs"):
        for panoid, image_paths, images in zip(panoid_batch, image_paths_batch, images_batch):
            # Move the images to the device
            images = [image.to(device) for image in images]
            
            # Calculate greenery indices for the batch
            greenery_indices = calculate_greenery_index_batch(images)
            
            # Append results for each image
            for image_path, greenery_index in zip(image_paths, greenery_indices):
                chunk_results.append([panoid, image_path.name, greenery_index])

    # Append chunk results to CSV
    df_chunk_results = pd.DataFrame(chunk_results, columns=['panoid', 'Image_Name', 'Greenery_Index'])
    df_chunk_results.to_csv(results_file, mode='a', header=not results_file.exists(), index=False)

# Main function to process all panoids iteratively
def main(panoids, batch_size=1000, results_file='Data/greenery_index_results_big.csv'):
    results_file = Path(results_file)
    processed_panoids = read_existing_results(results_file)
    total_panoids = len(panoids)
    already_processed = len(processed_panoids)

    print(f"Total Pano IDs: {total_panoids}")
    print(f"Already processed: {already_processed} ({(already_processed / total_panoids) * 100:.2f}%)")
    print(f"Remaining: {total_panoids - already_processed} ({((total_panoids - already_processed) / total_panoids) * 100:.2f}%)")

    # Filter out already processed panoids
    panoids_to_process = panoids[~panoids['panoid'].isin(processed_panoids)].reset_index(drop=True)
    num_batches = (len(panoids_to_process) + batch_size - 1) // batch_size

    processed_count = already_processed
    with tqdm(total=total_panoids, initial=already_processed, desc="Overall Progress") as pbar:
        for i in range(num_batches):
            start_idx = i * batch_size
            end_idx = min((i + 1) * batch_size, len(panoids_to_process))
            panoids_chunk = panoids_to_process.iloc[start_idx:end_idx]
            process_chunk(panoids_chunk, results_file)

            processed_count += len(panoids_chunk)
            pbar.update(len(panoids_chunk))

# Usage
if __name__ == "__main__":
    
    ### ===============================
    ### Process All Images
    ### ===============================
    print("Starting processing...")
    main(panoids, batch_size=100)
    
    ### ===============================
    ### For Testing with a Sample
    ### The progress bar will not show the correct progress here
    ### ===============================
    # panoids_sample = panoids.sample(100).reset_index(drop=True)
    # main(panoids_sample, batch_size=10)
