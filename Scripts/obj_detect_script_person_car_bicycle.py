import torch
from PIL import Image
from transformers import AutoImageProcessor, AutoModelForObjectDetection
import geopandas as gpd
from pathlib import Path
import pandas as pd
from tqdm import tqdm
from torch.utils.data import DataLoader, Dataset


# Check if MPS is available and set the device
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
device = "cpu"  # Forcing CPU for now
print(f"Using device: {device}")

# Load YOLO model for object detection
image_processor = AutoImageProcessor.from_pretrained("hustvl/yolos-tiny")
model = AutoModelForObjectDetection.from_pretrained("hustvl/yolos-tiny")
model.to(device)
print(f"Model {model.config.model_type} loaded successfully.")

# Define paths and load data
data_path = Path('/Users/wolff/Desktop/delft_download/data/Rotterdam_NL')
panoids = gpd.read_file(data_path / 'panoids/panoids.geojson')
print(f"Loaded panoids data with {len(panoids)} entries.")

### ============================================
### Filter Panoids: May to September, from 2017
### ============================================

# Apply the filtering criteria: Only use panoids between May and September, starting from the year 2017
panoids = panoids[
    (panoids['year'] >= 2017) & 
    (panoids['month'] >= 6) & 
    (panoids['month'] <= 9)
].reset_index(drop=True)

print(f"Filtered panoids data with {len(panoids)} entries remaining.")

### ============================================

# Function to get image paths for a given panoid
def get_image_paths(panoid):
    paths = panoids[panoids['panoid'] == panoid][['im_side_a', 'im_front', 'im_side_b', 'im_back']]
    paths = paths.values.tolist()[0]
    def img_path(x):
        return Path("/Users/wolff/Desktop/street_view_images") / f"{x}"
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
                images.append(Image.open(image_path).convert("RGB"))
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
    return list(panoids), list(image_paths_batch), list(images_batch)

# Function to calculate the count of people, cars, and bicycles for a batch of images
def calculate_object_counts_batch(images):
    counts = {'person': 0, 'car': 0, 'bicycle': 0}
    for image in images:
        inputs = image_processor(images=image, return_tensors="pt").to(device)
        with torch.no_grad():
            outputs = model(**inputs)
        results = image_processor.post_process_object_detection(outputs, threshold=0.90, target_sizes=[image.size[::-1]])[0]
        for label in results["labels"]:
            class_name = model.config.id2label[label.item()]
            if class_name in counts:
                counts[class_name] += 1
    return counts

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
            # Calculate object counts for the batch
            object_counts = calculate_object_counts_batch(images)
            
            # Append results for each image
            for image_path in image_paths:
                chunk_results.append([panoid, image_path.name, object_counts['person'], object_counts['car'], object_counts['bicycle']])

    # Append chunk results to CSV
    df_chunk_results = pd.DataFrame(chunk_results, columns=['panoid', 'Image_Name', 'count_people', 'count_cars', 'count_bicycles'])
    df_chunk_results.to_csv(results_file, mode='a', header=not results_file.exists(), index=False)

# Main function to process all panoids iteratively
def main(panoids, batch_size=1000, results_file='Data/object_detection_results_big.csv'):
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
    # main(panoids_sample, batch_size=100)



######## Checking the results by visualizing the detections ########


# from transformers import AutoImageProcessor, AutoModelForObjectDetection
# import torch
# from PIL import Image, ImageDraw, ImageFont
# import pandas as pd
# from pathlib import Path

# # Load the model and processor
# image_processor = AutoImageProcessor.from_pretrained("hustvl/yolos-tiny")
# model = AutoModelForObjectDetection.from_pretrained("hustvl/yolos-tiny")

# # Function to visualize detections from the CSV file
# def visualize_detections(csv_file, image_folder):
#     # Load the CSV file
#     df = pd.read_csv(csv_file)
    
#     # Define colors for each class
#     colors = {
#         'person': 'yellow',
#         'car': 'red',
#         'bicycle': 'blue'
#     }
    
#     # Get label ids for the classes of interest
#     label_ids = {class_name: {v: k for k, v in model.config.id2label.items()}[class_name] for class_name in colors.keys()}
    
#     # Iterate through the rows of the CSV file
#     for idx, row in df.iterrows():
#         panoid = row['panoid']
#         image_name = row['Image_Name']
        
#         # Load and prepare the image
#         image_path = Path(image_folder) / image_name
#         if not image_path.exists():
#             print(f"Image {image_path} not found. Skipping.")
#             continue
        
#         image = Image.open(image_path).convert("RGB")
        
#         # Process the image and generate predictions
#         inputs = image_processor(images=image, return_tensors="pt").to(device)
#         outputs = model(**inputs)
        
#         # Convert outputs to Pascal VOC format and filter for high confidence predictions
#         target_sizes = torch.tensor([image.size[::-1]])
#         results = image_processor.post_process_object_detection(outputs, threshold=0.90, target_sizes=target_sizes)[0]
        
#         # Initialize ImageDraw to draw on the image
#         draw = ImageDraw.Draw(image)
        
#         # Go through the detection results and draw bounding boxes for the classes of interest
#         for score, label, box in zip(results['scores'], results['labels'], results['boxes']):
#             class_name = model.config.id2label[label.item()]
#             if class_name in colors:
#                 # Scale box coordinates for visualization
#                 box = [round(i, 2) for i in box.tolist()]
#                 label_text = f"{class_name} {round(score.item(), 3)}"
                
#                 # Draw the bounding box
#                 draw.rectangle(box, outline=colors[class_name], width=2)
                
#                 # Draw the label text on the image
#                 draw.text((box[0], box[1] - 10), label_text, fill=colors[class_name])

#         # Display the image with bounding boxes
#         image.show()

# # Example usage
# csv_file = 'object_detection_results_big.csv'  # Path to your CSV file
# image_folder = '/Users/wolff/Desktop/street_view_images'  # Folder where images are stored

# visualize_detections(csv_file, image_folder)
