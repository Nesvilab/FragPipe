"""
Scripting module for downstream analysis of FPOP data following FragPipe search.
"""
import re
import pathlib
import numpy as np
import pandas as pd
import time
import sys
import io

# global constants
FPOP_MOD_MASSES = [15.994915, 31.989829, 47.984744, 13.979265, -43.053433, -22.031969, -23.015984, -10.031969, 4.9735,
                   -30.010565, -27.994915, -43.989829, -25.031631, -9.036716]
DESCRIPTION_ITEMS = [
    # 'Peptide Sequence',
    # 'Start', 'End',
    'Protein', 'Protein ID', 'Entry Name', 'Gene', 'Protein Description', 'Mapped Proteins', 'Mapped Genes']

# for testing/running from script directly
FILEPATH = r"Z:\crojaram\FPOP_Project\In-vivo_FPOP\Output\Manuscript\Group-filteredFDR\SSHII_Quant_lowercase\combined_modified_peptide.tsv"
CONTROL_LABEL = "Control_"
FPOP_LABEL = "Sample_"
REGION_SIZE = 1
SUBTRACT_CONTROL = False
# TMT = True


class Parameters(object):
    """
    container for parameters
    """
    modpep_tsv_path: str
    region_size: int
    control_label: str
    fpop_label: str
    subtract_control: bool
    # is_tmt: bool

    def __init__(self, tsvpath, region, control, fpop, subtract_control
                 # , is_tmt
                 ):
        self.modpep_tsv_path = tsvpath
        self.region_size = int(region)
        self.control_label = control
        self.fpop_label = fpop
        self.subtract_control = subtract_control
        # self.is_tmt = is_tmt


def parse_modified_peptide_tsv(filepath):
    """
    parse with pandas
    :param filepath: path to file to parse
    :type filepath: str
    :return: dataframe
    :rtype: pd.Dataframe
    """
    return pd.read_csv(filepath, index_col=False, sep='\t')


def group_peptides_better(mod_pep_df):
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
    # first pass: group peptides by protein and start position
    for index, row in mod_pep_df.iterrows():
        row['is_fpop'] = is_fpop(row)
        row['is_grouped'] = False
        protein = row['Protein ID']
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
                end = row['End']
                if end > highest_end:
                    highest_end = end
                    longest_pep = row['Peptide Sequence']
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
                        if row['Start'] < highest_end:
                            # found overlap, group with the others
                            row['is_grouped'] = True
                            peptide_groups[longest_pep].append(row)
                            found_overlapping_peptides = True
                            if row['End'] > highest_end:
                                highest_end = row['End']
                else:
                    found_overlapping_peptides = False

            # re-check for longest peptide in case an overlapping one was longer
            longest_length = len(longest_pep)
            new_pep = ''
            for row in peptide_groups[longest_pep]:
                if row['Peptide Length'] > longest_length:
                    longest_length = row['Peptide Length']
                    new_pep = row['Peptide Sequence']
            if new_pep != '':
                peptide_groups[new_pep] = peptide_groups.pop(longest_pep)

            protein_pep_groups[protein] = peptide_groups

    # final pass: filter to only peptides with at least 2 entries (single entry can't have both FPOP and unmodified)
    filtered_data = {}
    for protein, peptide_dict in protein_pep_groups.items():
        for peptide, row_list in peptide_dict.items():
            if len(row_list) > 1:
                filtered_data[peptide] = row_list

    return filtered_data


def group_peptides(mod_pep_df):
    """
    Deprecated.
    Group by base peptide sequence (peptide-level analysis). Returns a dict of group name (i.e., base peptide
    sequence): list of pandas Series for each relevant row of the input table. Also computes whether FPOP
    mods are present.
    :param mod_pep_df: modified peptide tsv dataframe
    :type mod_pep_df: pd.Dataframe
    :return: dict of group name: list of rows
    :rtype: dict
    """
    peptide_data = {}
    for index, row in mod_pep_df.iterrows():
        row['is_fpop'] = is_fpop(row)
        peptide = row['Peptide Sequence']
        if peptide in peptide_data.keys():
            peptide_data[peptide].append(row)
        else:
            peptide_data[peptide] = [row]

    # remove single-entry peptides (either unmodified only or modified only) because no comparative calculation can be done
    filtered_data = {}
    for peptide, row_list in peptide_data.items():
        if len(row_list) > 1:
            filtered_data[peptide] = row_list

    return filtered_data


def group_sites(mod_pep_df, region_size=1):
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
    site_data = {}
    protein_regions = {}
    unmod_rows = []  # holder to avoid regenerating a series for each unmodified peptide row
    # first pass: define modified regions and group modified peptides
    for index, row in mod_pep_df.iterrows():
        row['is_fpop'] = is_fpop(row)
        protein = row['Protein ID']
        start = row['Start']
        if row['is_fpop']:
            # check for all modification positions to determine the region to check against existing sites
            row['fpop_sites'] = get_all_fpop_mod_sites(row)
            protein_sites = [x + start for x in row['fpop_sites']]
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
        protein = row['Protein ID']
        start = row['Start']
        end = row['End']
        if not row['is_fpop']:
            # use start/end to determine region(s). Any peptide containing or within tolerance of a defined site/region is counted as belonging
            if protein in protein_regions.keys():
                for site in protein_regions[protein]:
                    if start - region_size < site < end + region_size:
                        site_data['{}_{}'.format(protein, site)].append(row)
    return site_data


def compute_group_mod_ratio(filtered_dict):
    """
    Take the peptide/site group dictionary and collapse the groups to modification (FPOP) ratios. Each group
    consists of FPOP and non-FPOP peptide entries. Ratio is total FPOP intensity / total intensity. Ratio calculation
    is performed for all samples individually.
    :param filtered_dict: dict of peptide/site: list of pd.Series with the combined_modified_peptide.tsv info for each row
    :type filtered_dict: dict
    :return: dict of group name: ratio dataframe with ratios for each sample column
    :rtype: dict
    """
    output = {}
    group_descriptions = {}
    sample_list = []
    for group_name, row_list in filtered_dict.items():
        # get descriptive info to save for output
        peptides = []
        longest_pep_index, highest_end, lowest_start = 0, 0, np.inf
        for index, row in enumerate(row_list):
            # use longest peptide description for peptide table; for site table, this conditional will always miss so will use index 0
            if row['Start'] < lowest_start:
                lowest_start = row['Start']
            if row['End'] > highest_end:
                highest_end = row['End']
            if row['Peptide Sequence'] == group_name:
                longest_pep_index = index
            peptides.append(row['Peptide Sequence'])
        group_descriptions[group_name] = row_list[longest_pep_index][DESCRIPTION_ITEMS]
        group_descriptions[group_name]['Peptide Sequence'] = ', '.join(set(peptides))
        group_descriptions[group_name]['Start'] = lowest_start
        group_descriptions[group_name]['End'] = highest_end
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


def is_fpop(mod_pep_row):
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


def get_all_fpop_mod_sites(mod_pep_row):
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
    peptides = group_peptides_better(mod_pep_df)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio(peptides)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent / 'FPOP_peptides.tsv'
    save_output(final_dict, group_descriptions, output_path)
    pep_time = time.time()
    print(" done in {:.1f}s".format(pep_time - start), flush=True)

    print("\tGenerating site-level table...", end='', flush=True)
    sites = group_sites(mod_pep_df, params.region_size)
    group_ratios, sample_list, group_descriptions = compute_group_mod_ratio(sites)
    if params.subtract_control:
        final_dict = compute_experiment_final_ratios(group_ratios, sample_list, params.control_label, params.fpop_label)
    else:
        final_dict = group_ratios
    output_path = pathlib.Path(params.modpep_tsv_path).parent / 'FPOP_sites.tsv'
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
        print('python3 FragPipe_FPOP_Analysis.py combined_modified_peptide.tsv 5 FPOP Control')
        sys.exit(0)

    params = Parameters(*argv)
    single_lfq_analysis(params)


def test():
    """
    Method for testing offline
    """
    params = Parameters(FILEPATH, REGION_SIZE, CONTROL_LABEL, FPOP_LABEL, SUBTRACT_CONTROL)
    # params = Parameters(FILEPATH, REGION_SIZE, CONTROL_LABEL, FPOP_LABEL, TMT)
    single_lfq_analysis(params)


if __name__ == '__main__':
    main()
    # test()
