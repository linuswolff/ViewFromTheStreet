############### other version with comparison of origin and destination files

import os
import smbclient
from concurrent.futures import ThreadPoolExecutor, as_completed
import sys
from tqdm import tqdm

smbclient.ClientConfig(username='xxx', password='xxx')

# Server details
server_name = "xxx"
server_share = "xxx"
server_path = "xxx"

def list_local_files(directory):
    """List all files in the local directory."""
    return set(os.listdir(directory))

def list_server_files(server_folder):
    """List all files in the server directory."""
    return set(smbclient.listdir(server_folder))

def get_files_to_copy(server_files, local_files):
    """Determine which files need to be copied."""
    return server_files - local_files  # This is based on the assumption that filenames are unique identifiers

def copy_file(file_name, server_folder, local_dest):
    """Copy a single file from server to local."""
    try:
        server_file_path = f"{server_folder}\\{file_name}"
        local_file_path = os.path.join(local_dest, file_name)
        
        with smbclient.open_file(server_file_path, mode="rb") as server_file, open(local_file_path, "wb") as local_file:
            local_file.write(server_file.read())
        #print(f"Successfully copied {file_name}")
    except Exception as e:
        print(f"Error copying {file_name}: {str(e)}")

def main():
    print("Setting up file lists...")
    server_folder = f"\\\\{server_name}\\{server_share}\\{server_path}\\data\\Rotterdam_NL\\imagedb"
    local_dest = os.path.join(os.path.expanduser('~'), 'Desktop', 'street_view_images')
    
    server_files = list_server_files(server_folder)
    print(f"Found {len(server_files)} files on server.")
    
    local_files = list_local_files(local_dest)
    print(f"Found {len(local_files)} files locally.")

    files_to_copy = get_files_to_copy(server_files, local_files)
    print(f"{len(files_to_copy)} files to copy.")
    
    print("Starting the file copy process...")
    
    with ThreadPoolExecutor(max_workers=7) as executor:
        futures = [executor.submit(copy_file, file, server_folder, local_dest) for file in files_to_copy]
        try:
            with tqdm(total=len(futures), desc="Copying files", unit="file") as progress:
                for future in as_completed(futures):
                    future.result()  # This ensures the exception, if any, gets raised
                    progress.update(1)  # Update the progress bar by one step per completed future
        except KeyboardInterrupt:
            print("Received exit, exiting...")
            for future in futures:
                future.cancel()  # Attempt to cancel all running tasks
            executor.shutdown(wait=False)  # Quick shutdown
            sys.exit(1)  # Exit program

if __name__ == "__main__":
    main()