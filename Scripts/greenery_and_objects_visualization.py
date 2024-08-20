# import libraries
import pandas as pd
import torch
from PIL import Image
from transformers import AutoImageProcessor, Mask2FormerForUniversalSegmentation
import numpy as np
import geopandas as gpd
from pathlib import Path
import matplotlib.pyplot as plt
from PIL import Image, ImageDraw, ImageFont

def img_path(x):
        return Path("/Users/wolff/Desktop/street_view_images") / f"{x}"

# Load Mask2Former fine-tuned on Cityscapes semantic segmentation
processor = AutoImageProcessor.from_pretrained("facebook/mask2former-swin-small-cityscapes-semantic")
model = Mask2FormerForUniversalSegmentation.from_pretrained("facebook/mask2former-swin-small-cityscapes-semantic")

# read in greenery index results
df = pd.read_csv('Data/greenery_index_results_big.csv')

# convert the greenery index to a float like this panoids_sample['Greenery_Index'] = panoids_sample['Greenery_Index'].str.strip('[]').astype(float)
df['Greenery_Index'] = df['Greenery_Index'].str.strip('[]').astype(float)

# image names we will use: image_113398_f.png, image_154880_s_a.png, image_180826_f.png, image_180979_s_b.png (good), image_188387_f.png as a sample
images_to_visualize = ['image_180826_f.png', 'image_77366_b.png', 'image_197502_f.png', 'image_188387_f.png']

# Initialize an empty list to collect rows
rows = []

# Iterate over the list of image names
for image_name in images_to_visualize:
    # Check if image_name exists in df
    if image_name in df['Image_Name'].values:
        # Get the Greenery_Index for the current image_name
        greenery_index = df.loc[df['Image_Name'] == image_name, 'Greenery_Index'].values[0]
        # Get the image path
        image_path = img_path(image_name)
        # Append the row to the list
        rows.append({'Image_Name': image_name, 'Greenery_Index': greenery_index, 'Image_Path': image_path})

# Create the DataFrame from the list of rows
df_visualization = pd.DataFrame(rows, columns=['Image_Name', 'Greenery_Index', 'Image_Path'])

# Display the DataFrame
print(df_visualization)



###### Functions to generate pngs of just the images and of the greenery masks
### images

def save_images_as_png(df_visualization):
    for index, row in df_visualization.iterrows():
        # Load the image
        image = Image.open(row['Image_Path']).convert("RGB")
        # Save the image as PNG
        image.save(row['Image_Name'])

# Example usage
save_images_as_png(df_visualization)

### masks

def save_greenery_masks(df_visualization):
    for index, row in df_visualization.iterrows():
        # Load the image
        image = Image.open(row['Image_Path']).convert("RGB")
        # Process the image and generate predictions
        inputs = processor(images=image, return_tensors="pt").to('cpu')
        with torch.no_grad():
            outputs = model(**inputs)
        # Post-process the outputs to get the semantic segmentation map
        predicted_semantic_map = processor.post_process_semantic_segmentation(outputs, target_sizes=[image.size[::-1]])[0]
        # Create a new mask with the same dimensions as the image
        mask = np.zeros((predicted_semantic_map.shape[0], predicted_semantic_map.shape[1], 3), dtype=np.uint8)
        # Identify indices for 'vegetation' and 'terrain'
        vegetation_index = [idx for idx, label in model.config.id2label.items() if label.lower() == 'vegetation'][0]
        terrain_index = [idx for idx, label in model.config.id2label.items() if label.lower() == 'terrain'][0]
        # Apply green color to vegetation and terrain
        mask[predicted_semantic_map.cpu() == vegetation_index] = [0, 255, 0]  # Green for vegetation
        mask[predicted_semantic_map.cpu() == terrain_index] = [0, 255, 0]  # Green for terrain
        # Save the mask as PNG
        mask_image = Image.fromarray(mask)
        mask_image.save(f'mask_{row["Image_Name"]}')

# Example usage
save_greenery_masks(df_visualization)





#################################################################################
#################################################################################
#################################################################################

# now the same for the object detection results

from transformers import AutoImageProcessor, AutoModelForObjectDetection
import torch
from PIL import Image, ImageDraw, ImageFont
import pandas as pd

# Load the model and processor
image_processor = AutoImageProcessor.from_pretrained("hustvl/yolos-tiny")
model = AutoModelForObjectDetection.from_pretrained("hustvl/yolos-tiny")

# import object_detection_results_big.csv

df = pd.read_csv('Data/object_detection_results_big.csv')

# subset the df only images where the person, car, and bicycle counts are > 0
df = df[df['count_people'] > 0]
df = df[df['count_cars'] > 0]
df = df[df['count_bicycles'] > 0]

## images to use: image_193329_b.png, image_181335_b.png, image_136525_f.png, image_198929_s_b.png, image_138033_b.png, image_261707_f.png 

images_to_visualize = [
    'image_181335_b.png', 'image_138033_b.png', 'image_261707_f.png', 'image_137488_f.png', 'image_133553_f.png', 'image_136507_s_b.png', 'image_3959_b.png', 'image_33233_f.png'
]

# Filter the DataFrame to include only the images to visualize
df_visualization = df[df['Image_Name'].isin(images_to_visualize)]


# Load the model and processor
image_processor = AutoImageProcessor.from_pretrained("hustvl/yolos-tiny")
model = AutoModelForObjectDetection.from_pretrained("hustvl/yolos-tiny")
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def visualize_detections(df_visualization, image_folder="/Users/wolff/Desktop/python_copy_2/", classes_to_show=None):
    # Define colors for each class
    colors = {
        'person': 'yellow',
        'car': 'red',
        'bicycle': 'blue'
    }
    
    # Get label ids for the classes of interest
    label_ids = {class_name: {v: k for k, v in model.config.id2label.items()}[class_name] for class_name in colors.keys()}
    
    # Set default classes to show if none provided
    if classes_to_show is None:
        classes_to_show = list(colors.keys())

    # Iterate through the rows of the DataFrame
    for idx, row in df_visualization.iterrows():
        image_name = row['Image_Name']
        
        # Load and prepare the image
        image_path = image_folder + image_name
        image = Image.open(image_path).convert("RGB")
        
        # Process the image and generate predictions
        inputs = image_processor(images=image, return_tensors="pt").to(device)
        with torch.no_grad():
            outputs = model(**inputs)
        
        # Convert outputs to Pascal VOC format and filter for high confidence predictions
        target_sizes = torch.tensor([image.size[::-1]]).to(device)
        results = image_processor.post_process_object_detection(outputs, threshold=0.90, target_sizes=target_sizes)[0]
        
        # Initialize ImageDraw to draw on the image
        draw = ImageDraw.Draw(image)
        
        # Go through the detection results and draw bounding boxes for the classes of interest
        for score, label, box in zip(results['scores'], results['labels'], results['boxes']):
            class_name = model.config.id2label[label.item()]
            if class_name in classes_to_show:
                # Scale box coordinates for visualization
                box = [round(i, 2) for i in box.tolist()]
                
                # Draw the bounding box
                draw.rectangle(box, outline=colors[class_name], width=4)

        # Save the image with bounding boxes
        image.save(f'bbox_{image_name}', quality=100)  # Save with higher resolution 

# Example usage
classes_to_show_per_image = {
    'image_193329_b.png': ['person', 'car'],
    'image_137488_f.png': ['car', 'bicycle', 'person'],
    'image_133553_f.png': ['car', 'bicycle', 'person'],
    'image_181335_b.png': ['car', 'bicycle'],
    'image_136507_s_b.png': ['car', 'bicycle', 'person'],
    'image_138033_b.png': ['car', 'bicycle', 'person'],
    'image_261707_f.png': ['car', 'bicycle', 'person'],
    'image_3959_b.png': ['car', 'bicycle', 'person'],
    'image_33233_f.png': ['car', 'bicycle', 'person']
}

for image_name, classes in classes_to_show_per_image.items():
    visualize_detections(df_visualization[df_visualization['Image_Name'] == image_name], classes_to_show=classes)
