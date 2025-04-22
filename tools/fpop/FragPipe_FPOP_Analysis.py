# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at

#   http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.    

"""
Scripting module for downstream analysis of FPOP data following FragPipe search.
"""
import os
import re
import pathlib
import numpy as np
import pandas as pd
import time
import sys
import io
from enum import Enum

# global constants
FPOP_MOD_MASSES = [15.994915, 31.989829, 47.984744, 13.979265, -43.053433, -22.031969, -23.015984, -10.031969, 4.9735,
                   -30.010565, -27.994915, -43.989829, -25.031631, -9.036716]
DESCRIPTION_ITEMS_LFQ = ['Protein', 'Protein ID', 'Entry Name', 'Gene', 'Protein Description', 'Mapped Proteins', 'Mapped Genes']
DESCRIPTION_ITEMS_TMT = ['Gene', 'ProteinID', 'MaxPepProb', 'ReferenceIntensity']


class Type(Enum):
    """
    LFQ or TMT
    """
    TMT = "tmt"
    LFQ = "lfq"


# lookup for column names in each spreadsheet type
COLUMN_NAMES = {
    'peptide': {Type.LFQ: 'Peptide Sequence',
                Type.TMT: 'Peptide'},
    'protein': {Type.LFQ: 'Protein ID',
                Type.TMT: 'ProteinID'},
    'start': {Type.LFQ: 'Start',
              Type.TMT: 'Start'},
    'end': {Type.LFQ: 'End',
            Type.TMT: 'End'},
    'mods': {Type.LFQ: 'Assigned Modifications',
             Type.TMT: 'Index'}
}


# for testing/running from script directly
FILEPATH = r"Z:\crojaram\FPOP_Project\In-vivo_FPOP\Output\Manuscript\Group-filteredFDR\SSHII_Quant_lowercase\combined_modified_peptide.tsv"
FILE_TMT_MOD = r"E:\_Software_Tests\FPOP\2023-08-28_2xTMTI-test\tmt-report\ratio_multi-site_None.tsv"
FILE_TMT_UNMOD = r"E:\_Software_Tests\FPOP\2023-08-28_2xTMTI-test\tmt-report-unmod\ratio_peptide_None.tsv"
CONTROL_LABEL = "D"
FPOP_LABEL = "V"
REGION_SIZE = 1
SUBTRACT_CONTROL = 'false'
TMT = 'true'


class Parameters(object):
    """
    container for parameters
    """
    modpep_tsv_path: str
    region_size: int
    control_label: str
    fpop_label: str
    subtract_control: bool
    is_tmt: bool
    unmod_tsv: str

    def __init__(self, tsvpath, region, control, fpop, subtract_control, is_tmt, unmod_tsv=None):
        self.modpep_tsv_path = tsvpath
        self.region_size = int(region)
        self.control_label = control
        self.fpop_label = fpop
        self.subtract_control = subtract_control.lower().strip() == 'true'
        self.is_tmt = is_tmt.lower().strip() == 'true'
        self.unmod_tsv = unmod_tsv
        self.check_file_paths()
        self.print_params()

    def check_file_paths(self):
        """
        make sure the passed files exist and give nice error messages if not
        """
        if not os.path.exists(self.modpep_tsv_path):
            print('Error: mod peptide file {} does not exist! Stopping analysis.'.format(self.modpep_tsv_path))
            sys.exit(1)
        if self.is_tmt and not os.path.exists(self.unmod_tsv):
            print('Error: unmodified peptide file {} does not exist! Stopping analysis. Please check that the second TMT-Integrator run finished and generated report files'.format(self.unmod_tsv))
            sys.exit(1)

    def print_params(self):
        """
        Print the parsed parameters
        :return:
        :rtype:
        """
        if self.is_tmt:
            type_str = 'TMT'
        else:
            type_str = 'LFQ'
        print(f'FPOP analysis parameters:\n'
              f'\tAnalysis type: {type_str}\n'
              f'\tSite Region Size: {self.region_size}\n'
              f'\tSubtract Control Oxidation: {self.subtract_control}')
        if self.subtract_control:
            print(f'\tControl Label: {self.control_label}\n'
                  f'\tFPOP Label: {self.fpop_label}')


def parse_modified_peptide_tsv(filepath):
    """
    parse with pandas
    :param filepath: path to file to parse
    :type filepath: str
    :return: dataframe
    :rtype: pd.Dataframe
    """
    return pd.read_csv(filepath, index_col=False, sep='\t')


def group_peptides_better(mod_pep_df, is_tmt):
    """
    Group by base peptide sequence (peptide-level analysis). All overlapping peptides are grouped together, returning
    the peptide with the longest sequence as group name. Returns a dict of group name (i.e., base peptide
    sequence): list of pandas Series for each relevant row of the input table. Also computes whether FPOP
    mods are present.
    :param mod_pep_df: modified peptide tsv dataframe
    :type mod_pep_df: pd.Dataframe
    :return: dict of group name: list of rows
    :rtype: dict
    """
    protein_pep_starts_dict = {}
    if is_tmt:
        col_type = Type.TMT
    else:
        col_type = Type.LFQ

    # first pass: group peptides by protein and start position
    for index, row in mod_pep_df.iterrows():
        if not is_tmt:
            row['is_fpop'] = is_fpop_lfq(row)
        row['is_grouped'] = False
        protein = row[COLUMN_NAMES['protein'][col_type]]
        start = row['Start']
        if protein in protein_pep_starts_dict.keys():
            if start in protein_pep_starts_dict[protein]:
                protein_pep_starts_dict[protein][start].append(row)
            else:
                protein_pep_starts_dict[protein][start] = [row]
        else:
            protein_pep_starts_dict[protein] = {start: [row]}

    # second pass: collapse overlapping peptides into groups by biggest peptide fully containing other(s)
    protein_pep_groups = {}
    for protein, start_pos_dict in protein_pep_starts_dict.items():
        peptide_groups = {}
        start_pos_list = []
        for start, row_list in sorted(start_pos_dict.items(), key=lambda x: x[0]):
            start_pos_list.append(row_list)

        for index, row_list in enumerate(start_pos_list):
            highest_end = 0
            longest_pep = ''
            pep_list_temp = []
            for row in row_list:
                if row['is_grouped']:
                    continue
                # same start guaranteed. Find longest peptide by largest 'end', group all under that peptide
                end = row[COLUMN_NAMES['end'][col_type]]
                if end > highest_end:
                    highest_end = end
                    longest_pep = row[COLUMN_NAMES['peptide'][col_type]]
                row['is_grouped'] = True
                pep_list_temp.append(row)
            # save all peptides to the group now that we know which is longest
            peptide_groups[longest_pep] = pep_list_temp

            found_overlapping_peptides = True
            while found_overlapping_peptides:
                if len(start_pos_list) > index + 1:
                    next_list = start_pos_list[index + 1]
                    index += 1
                    found_overlapping_peptides = False
                    for row in next_list:
                        overlap = highest_end - row[COLUMN_NAMES['start'][col_type]]
                        if overlap > len(row[COLUMN_NAMES['peptide'][col_type]]):
                            # found overlap, group with the others. Must have at least half the peptide overlapping to be grouped together
                            row['is_grouped'] = True
                            peptide_groups[longest_pep].append(row)
                            found_overlapping_peptides = True
                            if row[COLUMN_NAMES['end'][col_type]] > highest_end:
                                highest_end = row[COLUMN_NAMES['end'][col_type]]
                else:
                    found_overlapping_peptides = False

            # re-check for longest peptide in case an overlapping one was longer
            longest_length = len(longest_pep)
            new_pep = ''
            for row in peptide_groups[longest_pep]:
                pep = row[COLUMN_NAMES['peptide'][col_type]]
                new_len = len(new_pep)
                if new_len > longest_length:
                    longest_length = new_len
                    new_pep = pep
            if new_pep != '':
                peptide_groups[new_pep] = peptide_groups.pop(longest_pep)

            protein_pep_groups[protein] = peptide_groups

    # final pass: filter to only peptides with at least 2 entries (single entry can't have both FPOP and unmodified)
    filtered_data = {}
    for protein, peptide_dict in protein_pep_groups.items():
        for peptide, row_list in peptide_dict.items():
            if is_tmt:
                # make sure both modified and unmodified peptides are found before continuing
                found_mod = False
                found_unmod = False
                for row in row_list:
                    if row['is_fpop']:
                        found_mod = True
                        if found_unmod:
                            break
                    else:
                        found_unmod = True
                        if found_mod:
                            break
                if found_mod and found_unmod:
                    filtered_data[peptide] = row_list
            else:
                if len(row_list) > 1:
                    filtered_data[peptide] = row_list

    return filtered_data


def group_sites(mod_pep_df, region_size=1, is_tmt=False):
    """
    Group based on modification sites rather than peptide sequence. If a region size is specified, consider all fpop mods
    within a given region (span of amino acids) together as one group.
    :param mod_pep_df: modified peptide tsv dataframe
    :type mod_pep_df: pd.Dataframe
    :param region_size: number of AAs to group together if multiple modifications are found nearby
    :type region_size: int
    :return: dict of group name: list of rows
    :rtype: dict
    """
    if is_tmt:
        col_type = Type.TMT
    else:
        col_type = Type.LFQ
    site_data = {}
    protein_regions = {}
    unmod_rows = []  # holder to avoid regenerating a series for each unmodified peptide row
    # first pass: define modified regions and group modified peptides
    for index, row in mod_pep_df.iterrows():
        if not is_tmt:
            row['is_fpop'] = is_fpop_lfq(row)
        protein = row[COLUMN_NAMES['protein'][col_type]]
        start = row[COLUMN_NAMES['start'][col_type]]
        if row['is_fpop']:
            # check for all modification positions to determine the region to check against existing sites
            if is_tmt:
                row['fpop_sites'] = get_all_mod_sites_tmt(row)
                protein_sites = [x for x in row['fpop_sites']]      # TMTI sites are already converted to protein index
            else:
                row['fpop_sites'] = get_all_fpop_mod_sites_lfq(row)
                protein_sites = [x + start for x in row['fpop_sites']]      # IonQuant sites are relative to peptide

            for site in protein_sites:
                if protein in protein_regions.keys():
                    # check against all existing sites
                    found_sites = []
                    for existing_site in protein_regions[protein]:
                        if abs(existing_site - site) < region_size:
                            found_sites.append(existing_site)
                    if len(found_sites) > 0:
                        # existing site(s) found, add this row to it/them
                        for existing_site in found_sites:
                            site_data['{}_{}'.format(protein, existing_site)].append(row)
                    else:
                        # new site
                        protein_regions[protein].append(site)
                        site_data['{}_{}'.format(protein, site)] = [row]
                else:
                    # new protein
                    protein_regions[protein] = [site]
                    site_data['{}_{}'.format(protein, site)] = [row]
        else:
            unmod_rows.append(row)

    # second pass: group unmodified peptides by site/region
    for index, row in enumerate(unmod_rows):
        protein = row[COLUMN_NAMES['protein'][col_type]]
        start = row[COLUMN_NAMES['start'][col_type]]
        end = row[COLUMN_NAMES['end'][col_type]]
        if not row['is_fpop']:
            # use start/end to determine region(s). Any peptide containing or within tolerance of a defined site/region is counted as belonging
            if protein in protein_regions.keys():
                for site in protein_regions[protein]:
                    if start - region_size < site < end + region_size:
                        site_data['{}_{}'.format(protein, site)].append(row)
    return site_data


def compute_group_mod_ratio_lfq(filtered_dict):
    """
    Take the peptide/site group dictionary and collapse the groups to modification (FPOP) ratios. Each group
    consists of FPOP and non-FPOP peptide entries. Ratio is total FPOP intensity / total intensity. Ratio calculation
    is performed for all samples individually.
    :param filtered_dict: dict of peptide/site: list of pd.Series with the combined_modified_peptide.tsv info for each row
    :type filtered_dict: dict
    :return: dict of group name: ratio dataframe with ratios for each sample column
    :rtype: dict
    """
    col_type = Type.LFQ
    output = {}
    group_descriptions = {}
    sample_list = []
    for group_name, row_list in filtered_dict.items():
        # get descriptive info to save for output
        peptides = []
        longest_pep_index, highest_end, lowest_start = 0, 0, np.inf
        for index, row in enumerate(row_list):
            # use longest peptide description for peptide table; for site table, this conditional will always miss so will use index 0
            if row[COLUMN_NAMES['start'][col_type]] < lowest_start:
                lowest_start = row[COLUMN_NAMES['start'][col_type]]
            if row[COLUMN_NAMES['end'][col_type]] > highest_end:
                highest_end = row[COLUMN_NAMES['end'][col_type]]
            if row[COLUMN_NAMES['peptide'][col_type]] == group_name:
                longest_pep_index = index
            peptides.append(row[COLUMN_NAMES['peptide'][col_type]])

        group_descriptions[group_name] = row_list[longest_pep_index][DESCRIPTION_ITEMS_LFQ]
        group_descriptions[group_name][COLUMN_NAMES['peptide'][col_type]] = ', '.join(set(peptides))
        group_descriptions[group_name][COLUMN_NAMES['start'][col_type]] = lowest_start
        group_descriptions[group_name][COLUMN_NAMES['end'][col_type]] = highest_end
        group_descriptions[group_name]['FPOP Mods'] = []
        group_descriptions[group_name]['Other Mods'] = []

        # sum intensity across all FPOP and non-FPOP entires
        sample_list = [name for name in row_list[0].index if ' Intensity' in name]
        unmod_int = {x: 0 for x in sample_list}
        mod_int = {x: 0 for x in sample_list}
        ratio_dict = {}
        for row in row_list:
            mods = row['Assigned Modifications']
            if row['is_fpop']:
                if not pd.isna(mods):
                    for mod in mods.split(', '):
                        if mod not in group_descriptions[group_name]['FPOP Mods']:
                            group_descriptions[group_name]['FPOP Mods'].append(mod)
                for sample in sample_list:
                    mod_int[sample] += row[sample]
            else:
                if not pd.isna(mods):
                    for mod in mods.split(', '):
                        if mod not in group_descriptions[group_name]['Other Mods']:
                            group_descriptions[group_name]['Other Mods'].append(mod)
                for sample in sample_list:
                    unmod_int[sample] += row[sample]
        # generate ratios from sum of FPOP and non-FPOP intensities for each sample
        for sample in sample_list:
            if unmod_int[sample] == 0 and mod_int[sample] == 0:
                ratio_dict[sample] = np.nan
            else:
                ratio_dict[sample] = mod_int[sample] / (unmod_int[sample] + mod_int[sample])
        output[group_name] = ratio_dict
    return output, sample_list, group_descriptions


def compute_group_mod_ratio_tmt(filtered_dict):
    """
    Take the peptide/site group dictionary and collapse the groups to modification (FPOP) ratios. Each group
    consists of FPOP and non-FPOP peptide entries. Inputs are ratios calculated by TMT-integrator, and are
    combined within groups using either weighted average or median (as done in TMT-I). FPOP oxidation ratio
    is calculated as the aggregated modified ratio / aggregated unmodified ratio.

    NOTE: TMT-Integrator includes modified peptides in the unmod report (it's all peptides, not just unmodified),
    so the "unmod" ratios are actually totals.

    :param filtered_dict: dict of peptide/site: list of pd.Series with the combined_modified_peptide.tsv info for each row
    :type filtered_dict: dict
    :return: dict of group name: ratio dataframe with ratios for each sample column
    :rtype: dict
    """
    col_type = Type.TMT
    output = {}
    group_descriptions = {}
    sample_list = []
    for group_name, row_list in filtered_dict.items():
        # get descriptive info to save for output
        peptides = []
        longest_pep_index, highest_end, lowest_start = 0, 0, np.inf
        for index, row in enumerate(row_list):
            # use longest peptide description for peptide table; for site table, this conditional will always miss so will use index 0
            if row[COLUMN_NAMES['start'][col_type]] < lowest_start:
                lowest_start = row[COLUMN_NAMES['start'][col_type]]
            if row[COLUMN_NAMES['end'][col_type]] > highest_end:
                highest_end = row[COLUMN_NAMES['end'][col_type]]
            if row[COLUMN_NAMES['peptide'][col_type]] == group_name:
                longest_pep_index = index
            peptides.append(row[COLUMN_NAMES['peptide'][col_type]].upper())

        group_descriptions[group_name] = row_list[longest_pep_index][DESCRIPTION_ITEMS_TMT]
        group_descriptions[group_name][COLUMN_NAMES['peptide'][col_type]] = ', '.join(set(peptides))
        group_descriptions[group_name][COLUMN_NAMES['start'][col_type]] = lowest_start
        group_descriptions[group_name][COLUMN_NAMES['end'][col_type]] = highest_end
        group_descriptions[group_name]['FPOP Mods'] = []
        group_descriptions[group_name]['Other Mods'] = []

        # sum intensity across all FPOP and non-FPOP entires
        if 'ReferenceIntensity' in row_list[0].index:
            sample_start = row_list[0].index.get_loc('ReferenceIntensity')
        else:
            sample_start = row_list[0].index.get_loc('MaxPepProb')
        sample_list = row_list[0].index.values
        sample_list = [x for x in sample_list[sample_start + 1: -1] if x != 'is_fpop']
        unmod_ratios = {x: [] for x in sample_list}
        mod_ratios = {x: [] for x in sample_list}
        ratio_dict = {}
        for row in row_list:
            if row['is_fpop']:
                mods = get_all_mods_tmt(row)
                for mod in mods:
                    if mod not in group_descriptions[group_name]['FPOP Mods']:
                        group_descriptions[group_name]['FPOP Mods'].append(mod)
                for sample in sample_list:
                    mod_ratios[sample].append(row[sample])
            else:
                for sample in sample_list:
                    unmod_ratios[sample].append(row[sample])

        # generate sums of FPOP and non-FPOP intensities for each sample
        for sample in sample_list:
            mod_sum = np.sum(mod_ratios[sample])
            unmod_sum = np.sum(unmod_ratios[sample])
            if unmod_sum != 0:
                ratio_dict[sample] = mod_sum / unmod_sum
        output[group_name] = ratio_dict
    return output, sample_list, group_descriptions


def compute_experiment_final_ratios(group_ratios_dict, experiment_labels, control_label, fpop_label):
    """
    Collapse individual sample ratios for each peptide/site group by experiment by subtracting control sample
    ratios from FPOP sample ratios.
    :param group_ratios_dict: dict of group name: ratio dataframe
    :type group_ratios_dict: dict
    :param experiment_labels: list of all sample names
    :type experiment_labels: list
    :param control_label: label for control samples
    :type control_label: str
    :param fpop_label: label for FPOP samples
    :type fpop_label: str
    :return: dict of group name: experiment dataframe
    :rtype: dict
    """
    # pair up the experiment labels
    control_labels = {x.replace(control_label, ''): x for x in experiment_labels if control_label in x}
    fpop_labels = {x.replace(fpop_label, ''): x for x in experiment_labels if fpop_label in x}
    label_pairs = []
    for stripped_label in control_labels.keys():
        if stripped_label in fpop_labels.keys():
            label_pairs.append((control_labels[stripped_label], fpop_labels[stripped_label], stripped_label))
        else:
            print(f'warning: unpaired label {stripped_label}. This experiment will not be analyzed', flush=True)

    # Subtract the control ratio from FPOP ratio for each group
    output_dict = {}
    for group_name, ratio_dict in group_ratios_dict.items():
        final_ratio_dict = {}
        for label_pair in label_pairs:
            # save the stripped label (experiment name) with corrected ratio (fpop - control sample ratio)
            final_ratio_dict[label_pair[2]] = ratio_dict[label_pair[1]] - ratio_dict[label_pair[0]]
        output_dict[group_name] = final_ratio_dict
    return output_dict


def save_output(output_dict, group_descriptions, save_path):
    """
    save text file with output
    :param output_dict: dict of group name: ratio dict
    :type output_dict: dict
    :param group_descriptions: dict of group name: pd.Series with descriptive information
    :type group_descriptions: dict
    :param save_path: where to save
    :type save_path: pathlib.Path
    :return: void
    :rtype:
    """
    with open(save_path, 'w') as outfile:
        saved_header = False
        for group_name, ratio_dict in output_dict.items():
            if not saved_header:
                final_descriptions = ['Group Name']
                final_descriptions.extend([x for x in group_descriptions[group_name].index])
                header = '{}\t{}\n'.format('\t'.join(final_descriptions), '\t'.join([x.replace(' Intensity', '') for x in ratio_dict.keys()]))
                outfile.write(header)
                saved_header = True
            outfile.write('{}\t{}\t{}\n'.format(group_name, '\t'.join([to_string(x) for x in group_descriptions[group_name].to_list()]), '\t'.join([to_string(x) for x in ratio_dict.values()])))


def is_fpop_lfq(mod_pep_row):
    """
    Determine if a given row (pd.Series) contains FPOP mods or not
    :param mod_pep_row: row from combined_modified_peptide.tsv
    :type mod_pep_row: pd.Series
    :return: bool
    :rtype: bool
    """
    mods = mod_pep_row['Assigned Modifications']
    if pd.isna(mods):
        return False  # no modifications
    else:
        # parse mods and look for any FPOP mods from provided list
        mods = mods.split(',')
        for mod in mods:
            match = re.search(r"\((-?\d+\.\d+)\)", mod)
            if match:
                mass = float(match.group(1))
                for fpop_mass in FPOP_MOD_MASSES:
                    if abs(mass - fpop_mass) < 0.001:
                        return True
        # no match found
        return False


def get_all_fpop_mod_sites_lfq(mod_pep_row):
    """

    :param mod_pep_row:
    :type mod_pep_row:
    :return:
    :rtype:
    """
    mods = mod_pep_row['Assigned Modifications'].split(',')
    fpop_sites = []
    for mod in mods:
        match = re.search(r"\((-?\d+\.\d+)\)", mod)
        if match:
            mass = float(match.group(1))
            for fpop_mass in FPOP_MOD_MASSES:
                if abs(mass - fpop_mass) < 0.001:
                    # fpop mod
                    splits = mod.split('(')
                    location = int(re.search(r"(\d+)", splits[0]).group(1))
                    fpop_sites.append(location)
                    break
    return fpop_sites


def get_all_mod_sites_tmt(mod_pep_row):
    """
    Parse index of multi-site report to get all mod sites in this entry and return them as a list
    :param mod_pep_row:
    :type mod_pep_row:
    :return:
    :rtype:
    """
    mod_str = mod_pep_row[COLUMN_NAMES['mods'][Type.TMT]].split('_')[-1]
    fpop_sites = []
    pattern = re.compile(r"[A-Z][0-9]+")
    for mod in re.findall(pattern, mod_str):
        location = int(re.search(r"(\d+)", mod).group(1))
        fpop_sites.append(location)
    return fpop_sites


def get_all_mods_tmt(mod_pep_row):
    """
    Parse index of multi-site report to get all mods in this entry and return them as a set
    :param mod_pep_row:
    :type mod_pep_row:
    :return:
    :rtype:
    """
    mod_str = mod_pep_row[COLUMN_NAMES['mods'][Type.TMT]].split('_')[-1]
    fpop_sites = []
    pattern = re.compile(r"[A-Z][0-9]+")
    for mod in re.findall(pattern, mod_str):
        fpop_sites.append(mod)
    return set(fpop_sites)


def to_string(x):
    """
    overwriting str() for nicer printing of some cases
    :param x: input str
    :type x: str
    :return: str
    :rtype: str
    """
    if isinstance(x, list):
        if len(x) == 0:
            return ''
        else:
            return ', '.join(x)
    elif pd.isna(x):
        return ''
    else:
        return str(x)


def single_lfq_analysis(params):
    """
    Given a combined_modified_peptide.tsv from IonQuant, perform FPOP quant analysis
    :return:
    :rtype:
    """
    start = time.time()
    print("Analyzing FPOP data for file {}".format(params.modpep_tsv_path), flush=True)
    print("\tParsing input file...", end='', flush=True)
    mod_pep_df = parse_modified_peptide_tsv(params.modpep_tsv_path)
    print(" done in {:.1f}s".format(time.time() - start), flush=True)

    print("\tGenerating peptide-level table...", end='', flush=True)
    peptides = group_peptides_better(mod_pep_df, params.is_tmt)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio_lfq(peptides)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent / 'FPOP_peptides.tsv'
    save_output(final_dict, group_descriptions, output_path)
    pep_time = time.time()
    print(" done in {:.1f}s".format(pep_time - start), flush=True)

    print("\tGenerating site-level table...", end='', flush=True)
    sites = group_sites(mod_pep_df, params.region_size, params.is_tmt)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio_lfq(sites)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent / 'FPOP_sites.tsv'
    save_output(final_dict, group_descriptions, output_path)
    print("\t done in {:.1f}s".format(time.time() - pep_time), flush=True)
    print("Done in {:.1f}s".format(time.time() - start), flush=True)


def single_tmt_analysis(params):
    """
    Given both modified and unmodified peptide and site level TMT-Integrator reports, perform FPOP quant analysis
    :return:
    :rtype:
    """
    start = time.time()
    print("Analyzing FPOP data for file {}".format(params.modpep_tsv_path), flush=True)
    print("\tParsing input file...", end='', flush=True)
    mod_pep_df = parse_modified_peptide_tsv(params.modpep_tsv_path)
    mod_pep_df['is_fpop'] = True
    unmod_pep_df = parse_modified_peptide_tsv(params.unmod_tsv)
    unmod_pep_df['is_fpop'] = False
    mod_pep_df = pd.concat([mod_pep_df, unmod_pep_df])
    print(" done in {:.1f}s".format(time.time() - start), flush=True)

    print("\tGenerating peptide-level table...", end='', flush=True)
    peptides = group_peptides_better(mod_pep_df, params.is_tmt)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio_tmt(peptides)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent.parent / 'FPOP_peptides.tsv'
    save_output(final_dict, group_descriptions, output_path)
    pep_time = time.time()
    print(" done in {:.1f}s".format(pep_time - start), flush=True)

    print("\tGenerating site-level table...", end='', flush=True)
    sites = group_sites(mod_pep_df, params.region_size, params.is_tmt)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio_tmt(sites)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent.parent / 'FPOP_sites.tsv'
    save_output(final_dict, group_descriptions, output_path)
    print("\t done in {:.1f}s".format(time.time() - pep_time), flush=True)
    print("Done in {:.1f}s".format(time.time() - start), flush=True)


def main():
    """
    Command line entry point
    :return: void
    :rtype:
    """
    if sys.version_info[:2] >= (3, 7):
        sys.stdout.reconfigure(encoding='utf-8')
        sys.stderr.reconfigure(encoding='utf-8')
    elif sys.version_info[:2] >= (3, 1):
        sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', line_buffering=True)
        sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', line_buffering=True)
    else:
        raise Exception('Python 3.1 or above is required')

    argv = sys.argv[1:]
    if len(argv) == 0:
        print('Example usage:')
        print('python3 FragPipe_FPOP_Analysis.py [modified peptide tsv] [site region size (int)] [fpop label] [control label] [subtract control (bool)] [is tmt (bool)] [unmodified peptide tsv (TMT analyses only)]')
        sys.exit(0)

    params = Parameters(*argv)
    if params.is_tmt:
        single_tmt_analysis(params)
    else:
        single_lfq_analysis(params)


def test():
    """
    Method for testing offline
    """
    params = Parameters(FILE_TMT_MOD, REGION_SIZE, CONTROL_LABEL, FPOP_LABEL, SUBTRACT_CONTROL, TMT, unmod_tsv=FILE_TMT_UNMOD)
    # params = Parameters(FILEPATH, REGION_SIZE, CONTROL_LABEL, FPOP_LABEL, TMT)
    if params.is_tmt:
        single_tmt_analysis(params)
    else:
        single_lfq_analysis(params)


if __name__ == '__main__':
    main()
    # test()
