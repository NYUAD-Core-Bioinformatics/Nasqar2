

# pip install PyYAML

import os
import yaml
import sys
from collections import defaultdict


applications = ['animalcules', 'ClusterProfShinyGSEA', 'debrowser-master', 'ATACseqQCShniy','ClusterProfShinyORA', 'deseq2shiny','GeneCountMerger',
        'monocle3',  'dada2Shiny', 'SeuratV5Shiny']
def merge_yaml_files(yaml_files):
    merged_data = {
        'name': 'merged_environment',
        'channels': [],
        'dependencies': []
    }
    
    channels_set = set()
    dependencies_set = set()
    
    for file in yaml_files:
        with open(file, 'r') as f:
            data = yaml.safe_load(f)
            channels_set.update(data.get('channels', []))
            dependencies_set.update(data.get('dependencies', []))
    
    merged_data['channels'] = list(channels_set)
    merged_data['dependencies'] = list(dependencies_set)

    print(merged_data)
    
    return merged_data

def save_merged_yaml(merged_data, output_file):
    with open(output_file, 'w') as f:
        yaml.safe_dump(merged_data, f, default_flow_style=False)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python merge_yaml.py output_file.yaml ")
        sys.exit(1)
    
    output_file = sys.argv[1]
    yaml_files = [app + '/' + 'environment.yaml'  for app in applications]
    
    merged_data = merge_yaml_files(yaml_files)
    save_merged_yaml(merged_data, output_file)

    print(f"Merged YAML saved to {output_file}")

