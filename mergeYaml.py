

# pip install PyYAML

import os
import yaml
import sys
from collections import defaultdict, OrderedDict


applications = ['animalcules', 'ClusterProfShinyGSEA', 'ATACseqQCShniy','ClusterProfShinyORA', 'deseq2shiny','GeneCountMerger',
        'monocle3',  'dada2Shiny', 'SeuratV5Shiny', 'DEBrowser', 'mergeFPKMs', 'tsar_nasqar']


import yaml

def merge_yaml_files(yaml_files):
    merged_data = {}

    channels_set = set()
    dependencies_set = set()

    for file in yaml_files:
        with open(file, 'r') as f:
            data = yaml.safe_load(f)
            channels_set.update(data.get('channels', []))
            dependencies_set.update(data.get('dependencies', []))

    # Ensure 'r-base==4.3.3' is the first dependency
    dependencies_sorted = ['r-base==4.3.3']

    # Sort and add dependencies starting with 'r-' and then 'bioconductor-'
    r_dependencies = sorted([dep for dep in dependencies_set if dep.startswith('r-')])
    bioconductor_dependencies = sorted([dep for dep in dependencies_set if dep.startswith('bioconductor-')])

    # Add the sorted dependencies to the list
    dependencies_sorted.extend(r_dependencies)
    dependencies_sorted.extend(bioconductor_dependencies)

    # Add any remaining dependencies that don't start with 'r-' or 'bioconductor-'
    other_dependencies = sorted([dep for dep in dependencies_set if not (dep.startswith('r-') or dep.startswith('bioconductor-'))])
    dependencies_sorted.extend(other_dependencies)

    merged_data['name'] = 'merged_env'
    merged_data['channels'] = sorted(list(channels_set))  # Sorted list of channels
    merged_data['dependencies'] = dependencies_sorted

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

