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

import os
import re
import argparse
import datetime as dt
import pandas as pd
import matplotlib

matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from io import BytesIO
from PyPDF2 import PdfMerger
from PIL import Image, ImageOps
import numpy as np

NATURE_PALETTE = ["#808080", "#A9A9A9"]  # grey colors
DEFAULT_STYLE = 'seaborn-v0_8-whitegrid'  # Simple white style


def compute_auto_width(series, min_width=50, char_width=10):
    max_len = series.astype(str).str.len().max()
    return max(min_width, max_len * char_width)


def create_distribution_plot(data=None, item="", ax=None):
    """Create histogram using matplotlib"""
    if ax is None:
        fig, ax = plt.subplots(figsize=(8, 6))

    data_sorted = data.sort_values(by=item)
    bins_num = 50 if item == "Peptide Length" else 10

    if item in ["Charge", "Number of Missed Cleavages"]:
        # For categorical data, create bar plot
        value_counts = data_sorted[item].value_counts().sort_index()
        ax.bar(value_counts.index.astype(str), value_counts.values,
               color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
    else:
        # For continuous data, create histogram
        ax.hist(data_sorted[item].dropna(), bins=bins_num,
                color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)

    ax.set_xlabel(item)
    ax.set_ylabel('Count')
    ax.grid(True, alpha=0.3)

    return ax


def create_bar_chart(data=None, x="", y="", ax=None):
    """Create bar chart using matplotlib"""
    if ax is None:
        fig, ax = plt.subplots(figsize=(8, 6))

    bars = ax.bar(data[x].astype(str), data[y],
                  color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)

    # Add text labels on bars
    for bar in bars:
        height = bar.get_height()
        ax.text(bar.get_x() + bar.get_width() / 2., height,
                f'{int(height)}',
                ha='center', va='bottom', fontsize=9)

    ax.set_xlabel(x)
    ax.set_ylabel(y)
    ax.grid(True, alpha=0.3, axis='y')

    return ax


def create_mass_error_plot(data, ax=None):
    """Create grouped bar plot for mass error"""
    if ax is None:
        fig, ax = plt.subplots(figsize=(10, 6))

    # Group data by Calib
    groups = data.groupby("Calib")
    x = np.arange(len(data["Run"].unique()))
    width = 0.35

    for i, (calib_name, grp) in enumerate(groups):
        offset = width * (i - 0.5)
        ax.bar(x + offset, grp["Value"], width,
               label=calib_name, color=NATURE_PALETTE[i % len(NATURE_PALETTE)],
               edgecolor='black', alpha=0.7)

    ax.set_xlabel('Run')
    ax.set_ylabel('Value')
    ax.set_title('Grouped Bar Plot')
    ax.set_xticks(x)
    ax.set_xticklabels(grp["Run"].unique())
    ax.legend()
    ax.grid(True, alpha=0.3, axis='y')

    return ax


def create_exp_page_running(all_data_dicts, run_name):
    """Create 3x3 subplot grid using matplotlib"""
    fig, axes = plt.subplots(3, 3, figsize=(16, 18))
    fig.suptitle(f'{run_name} Statistics', fontsize=16, fontweight='bold')

    titles = [
        "Peptide Length/# PSM", "Missed Cleavage/# PSM", "Charge/# PSM",
        "MZ/# PSM", "RT/# PSM", "Hyperscore/# PSM",
        "Delta Mass Dis", "MZ/Delta Mass", "RT/MZ"
    ]

    xlabels = [
        "Peptide Length", "Missed Cleavage", "Charge",
        "M/Z", "Retention Time", "Hyperscore",
        "Delta Mass (Da)", "M/Z", "Retention Time"
    ]

    ylabels = [
        "# PSM", "# PSM", "# PSM",
        "# PSM", "# PSM", "# PSM",
        "# PSM", "Delta Mass (Da)", "M/Z"
    ]

    for idx, (data_dict, title, xlabel, ylabel) in enumerate(zip(all_data_dicts, titles, xlabels, ylabels)):
        row = idx // 3
        col = idx % 3
        ax = axes[row, col]

        # Determine plot type based on data
        if 'type' in data_dict and data_dict['type'] == 'histogram':
            ax.hist(data_dict['data'], bins=data_dict.get('bins', 30),
                    color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
        elif 'type' in data_dict and data_dict['type'] == 'scatter':
            ax.scatter(data_dict['x'], data_dict['y'],
                       alpha=0.5, s=10, color=NATURE_PALETTE[0])
        else:
            # Default histogram from data
            if 'data' in data_dict:
                ax.hist(data_dict['data'], bins=30,
                        color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)

        ax.set_title(title, fontsize=10, fontweight='bold')
        ax.set_xlabel(xlabel, fontsize=9)
        ax.set_ylabel(ylabel, fontsize=9)
        ax.grid(True, alpha=0.3)

    plt.tight_layout()
    return fig


def fig_to_pdf_bytes(fig):
    """Convert matplotlib figure to PDF bytes"""
    pdf_buffer = BytesIO()
    fig.savefig(pdf_buffer, format='pdf', bbox_inches='tight', dpi=100)
    pdf_buffer.seek(0)
    plt.close(fig)
    return pdf_buffer


def fig_to_png_bytes(fig):
    """Convert matplotlib figure to PNG bytes"""
    png_buffer = BytesIO()
    fig.savefig(png_buffer, format='png', bbox_inches='tight', dpi=200)
    png_buffer.seek(0)
    plt.close(fig)
    return png_buffer


def merge_pdf_buffers(pdf_buffers):
    merger = PdfMerger()
    for buf in pdf_buffers:
        merger.append(buf)
        buf.seek(0)
    output = BytesIO()
    merger.write(output)
    merger.close()
    output.seek(0)
    return output


class FragPipeReport:

    def __init__(self, results_path=""):
        self.ms2_mass_df = None
        self.ms1_mass_df = None
        self.percolator_raw_file = None
        self.ms1_tolerance = None
        self.ms2_tolerance = None
        self.ms2_units = None
        self.ms1_units = None
        self.runtime_dict = None
        self.finish_time = None
        self.features_weight = None
        self.run_spec_lib = None
        self.manifest_data = None
        self.workflow_file = None
        self.latest_log_file = None
        self.runs_data = {}
        self.id_nums = None
        self.title = "FragPipe Report"
        self.results_path = results_path
        self.top_plots = []
        self.exp_plots = []
        self.session2_plots = {}
        self.run_data = {}
        self.single_run_data = {}
        self.msbooster_plots = {}
        self.distribution_data = pd.DataFrame(columns=['Peptide Length', 'Charge', 'Number of Missed Cleavages', "Exp"])
        self.read_data()
        self.get_running_time()
        self.get_mass_error()
        self.get_percolator_features()
        self.process_psm()

    def read_data(self):
        # Read the data
        log_files = []
        for file in os.listdir(self.results_path):
            if file == "fragpipe-files.fp-manifest":
                self.manifest_data = pd.read_csv(os.path.join(self.results_path, file), sep="\t", header=None,
                                                 dtype=str)
            if file == "fragpipe.workflow":
                self.workflow_file = os.path.join(self.results_path, file)
            if file.startswith("log_"):
                log_files.append(file)

        log_files.sort()
        if len(log_files) == 0:
            print("Error: No log file found in the results path: {}. Could not generate summary report".format(self.results_path))
            raise FileNotFoundError("Log file not found in the results path {}".format(self.results_path))
        self.latest_log_file = log_files[-1]

        with open(os.path.join(self.results_path, self.latest_log_file), 'r') as f:
            log_lines = f.readlines()
        for line in log_lines:
            if line.startswith("speclibgen.run-speclibgen"):
                if line.split("run-speclibgen=")[1].strip() == "false":
                    self.run_spec_lib = False
                else:
                    self.run_spec_lib = True
                break

        self.manifest_data.columns = ["Spectrum File", "Experiment", "Bioreplicate", "Data Type"]
        self.manifest_data.insert(0, 'Run', range(1, len(self.manifest_data) + 1))

        self.id_nums = pd.DataFrame(columns=["Experiment", "PSM", "Peptides", "Proteins"])
        # Combine psm, peptide, protein ids dataframe to one
        self.id_nums = pd.concat([self.id_nums, self.read_psm()], ignore_index=True)
        self.id_nums = pd.concat([self.id_nums, self.read_peptides()], ignore_index=True)
        self.id_nums = pd.concat([self.id_nums, self.read_proteins()], ignore_index=True)

    def get_percolator_features(self):
        with open(os.path.join(self.results_path, self.latest_log_file), "r") as f:
            log_text = f.read()

        pattern = (
            r"Learned normalized SVM weights for the 3 cross-validation splits:\s*\n"
            r"(.*?)\nFound\s+\d+"
        )
        blocks = re.finditer(pattern, log_text, re.DOTALL)

        self.features_weight = {}
        for i, blk in enumerate(blocks, start=1):
            block_text = blk.group(1).strip()
            lines = block_text.splitlines()
            header = [h.strip() for h in lines[0].split("\t")]
            # Data rows
            data = []
            for row in lines[1:]:
                if not row.strip():
                    continue
                cells = [c.strip() for c in row.split("\t")]
                data.append(cells)

            df = pd.DataFrame(data, columns=header)
            for col in ("Split1", "Split2", "Split3"):
                df[col] = df[col].astype(float)
            df["Mean Weight"] = df[["Split1", "Split2", "Split3"]].mean(axis=1).round(4)
            df = df[(df[["Split1", "Split2", "Split3"]] != 0).any(axis=1)].copy()
            df = df[["FeatureName", "Mean Weight", "Split1", "Split2", "Split3"]]
            df["abs_mean_col"] = df["Mean Weight"].abs()
            df = df.sort_values("abs_mean_col", ascending=False)
            df = df.drop(columns=["abs_mean_col"])
            df = df.drop(columns=["Split1"])
            df = df.drop(columns=["Split2"])
            df = df.drop(columns=["Split3"])
            df = df[~df['FeatureName'].str.contains("m0")]

            self.features_weight[self.percolator_raw_file[i - 1]] = df

        times = re.findall(r'time="(\d{2}):(\d{2}):(\d{2})"', log_text)
        times.extend(re.findall(r'\[\d{4}/\d{2}/\d{2} (\d{2}):(\d{2}):(\d{2})\]', log_text))

        # 2) convert to datetime.time objects
        td_list = [
            dt.timedelta(hours=int(h), minutes=int(m), seconds=int(s))
            for h, m, s in times
        ]

        creation_timestamp = os.path.getctime(os.path.join(self.results_path, self.latest_log_file))
        creation_datetime = dt.datetime.fromtimestamp(creation_timestamp)

        if not td_list:
            self.finish_time = None
        else:
            self.finish_time = dt.datetime.combine(creation_datetime.date(), dt.time()) + max(td_list)

    def get_running_time(self):
        self.runtime_dict = {}
        fragger_time = {}
        running_time_region = False
        main_search_region = False
        with open(os.path.join(self.results_path, self.latest_log_file), 'r') as f:
            log_lines = f.readlines()
        for line in log_lines:

            if not main_search_region:
                if line.startswith("precursor_true_tolerance = "):
                    self.ms1_tolerance = float(line.split(" = ")[1].strip())
                if line.startswith("fragment_mass_tolerance = "):
                    self.ms2_tolerance = float(line.split(" = ")[1].strip())
                if line.startswith("precursor_true_units = "):
                    self.ms1_units = line.split(" = ")[1].strip()
                if line.startswith("fragment_mass_units = "):
                    self.ms2_units = line.split(" = ")[1].strip()

            if line.startswith("***************************FIRST SEARCH DONE IN"):
                fragger_time["First Search"] = float(line.split("DONE IN ")[1].split(" MIN")[0].strip())

            if line.startswith("************MASS CALIBRATION AND PARAMETER OPTIMIZATION DONE IN"):
                fragger_time["Mass Calibration and<br>Parameter Optimization"] = float(line.split("DONE IN ")[1].split(" MIN")[0].strip())
                main_search_region = True
            if line.startswith("**************************MASS CALIBRATION DONE "):
                fragger_time["Mass Calibration"] = float(
                    line.split("DONE IN ")[1].split(" MIN")[0].strip())
            if line.startswith("***************************MAIN SEARCH DONE IN"):
                fragger_time["Main Search"] = float(line.split("DONE IN ")[1].split(" MIN")[0].strip())

            if line.startswith("Task Runtimes"):
                running_time_region = True
                continue
            if running_time_region:
                line_stripped = line.strip()

                task, minutes = line_stripped.rsplit(":", 1)
                if task == "MSFragger":
                    for key, value in fragger_time.items():
                        self.runtime_dict[key] = value
                if float(minutes.split()[0]) != 0 and not task.startswith("Percolator") and \
                        task not in ["MSFragger", "WorkspaceCleanInit", "WorkspaceClean"]:
                    self.runtime_dict[task.strip()] = float(minutes.split()[0])

                if task.startswith("Percolator"):
                    if "Percolator" not in self.runtime_dict:
                        self.runtime_dict["Percolator"] = 0
                    self.runtime_dict["Percolator"] += float(minutes.split()[0])

                if task == "Finalizer Task":
                    break
        if self.ms1_units == "2":
            if self.ms1_tolerance is not None:      # avoid crash if MSFragger has not been run
                self.ms1_tolerance = self.ms1_tolerance * 1000
        if self.ms2_units == "2":
            if self.ms2_tolerance is not None:
                self.ms2_tolerance = self.ms2_tolerance * 1000

    def get_mass_error(self):
        mass_error_region = False
        data_rows = []
        self.percolator_raw_file = []
        with open(os.path.join(self.results_path, self.latest_log_file), 'r') as f:
            log_lines = f.readlines()

        mass_cal_part = 0
        for line in log_lines:

            if line.startswith("*********************MASS CALIBRATION AND PARAMETER OPTIMIZATION*******************") or \
                    line.startswith("*********************************MASS CALIBRATION**"):
                mass_error_region = True
                mass_cal_part += 1
            if mass_error_region:
                line_stripped = line.strip()
                # Match lines that start with one or more digits (like "001")
                if re.match(r"^\d+", line_stripped):
                    data_rows.append(line_stripped + "|" + str(mass_cal_part))
            if line.startswith("Finding the optimal parameters:") or \
                    line.startswith("**************************MASS CALIBRATION DONE"):
                mass_error_region = False

            if line.startswith("Reading tab-delimited input from datafile"):
                full_name = line.strip().split("Reading tab-delimited input from datafile ")[-1]
                self.percolator_raw_file.append(full_name.split("_edited.pin")[0])

        ms1_rows = []
        ms2_rows = []
        for row in data_rows:
            parts = row.split("|")
            if len(parts) < 5:
                # Skip rows that don't have the expected number of columns.
                continue

            # The first column is the Run number.
            run = "Part " + parts[5].strip() + ": " + parts[0].strip()

            ms1_old = parts[1].strip().split()
            ms1_new = parts[2].strip().split()
            ms2_old = parts[3].strip().split()
            ms2_new = parts[4].strip().split()

            if len(ms1_old) == 2 and len(ms1_new) == 2 and len(ms2_old) == 2 and len(ms2_new) == 2:
                ms1_row_dict = {
                    "Run": run,
                    "Type": "Median",
                    "Calib": "Old",
                    "Value": float(ms1_old[0])
                }
                ms1_rows.append(ms1_row_dict)

                ms1_row_dict = {
                    "Run": run,
                    "Type": "Median",
                    "Calib": "Calibrated",
                    "Value": float(ms1_new[0])
                }
                ms1_rows.append(ms1_row_dict)

                ms2_row_dict = {
                    "Run": run,
                    "Type": "Median",
                    "Calib": "Old",
                    "Value": float(ms2_old[0])
                }
                ms2_rows.append(ms2_row_dict)

                ms2_row_dict = {
                    "Run": run,
                    "Type": "Median",
                    "Calib": "Calibrated",
                    "Value": float(ms2_new[0])
                }
                ms2_rows.append(ms2_row_dict)

        self.ms1_mass_df = pd.DataFrame(ms1_rows)
        self.ms2_mass_df = pd.DataFrame(ms2_rows)

    def count_lines(self, filename, exp=""):
        data_table = pd.read_csv(filename, sep="\t", on_bad_lines="skip",
                                 engine="pyarrow")  # TODO Find a better way to read the file with wrong numbers of columns

        if exp != "":
            distinct_rows = data_table.drop_duplicates(
                subset=['Spectrum', 'Modified Peptide', 'Peptide Length', 'Charge', 'Number of Missed Cleavages'])
            distinct_rows = distinct_rows[['Spectrum', 'Modified Peptide', 'Peptide Length', 'Charge', 'Number of Missed Cleavages']]
            del distinct_rows['Modified Peptide']
            del distinct_rows['Spectrum']
            distinct_rows = distinct_rows.assign(Exp=exp)
            self.distribution_data = pd.concat([self.distribution_data, distinct_rows], ignore_index=True)

        return data_table.shape[0]

    def read_psm(self):
        # Read the PSM data

        if os.path.exists(os.path.join(self.results_path, "MSBooster", "MSBooster_plots")):
            msbooster_files = os.listdir(os.path.join(self.results_path, "MSBooster", "MSBooster_plots"))
            for one_folder in msbooster_files:
                # check if the folder is a directory
                if not os.path.isdir(os.path.join(self.results_path, "MSBooster", "MSBooster_plots", one_folder)):
                    continue
                for file in os.listdir(os.path.join(self.results_path, "MSBooster", "MSBooster_plots", one_folder)):
                    if file.endswith(".png"):
                        if "edited" in file:
                            run_name = file.split("_edited")[0]
                        else:
                            run_name = file.split(".png")[0]
                        if run_name not in self.msbooster_plots:
                            self.msbooster_plots[run_name] = []
                        if one_folder == "RT_calibration_curves":
                            self.msbooster_plots[run_name].append(
                                Image.open(
                                    os.path.join(self.results_path, "MSBooster", "MSBooster_plots", one_folder, file)))
                        if "delta_RT_loess" in file or "pred_RT_real_units" in file or "unweighted_spectral_entropy" in file:
                            self.msbooster_plots[run_name].append(
                                Image.open(
                                    os.path.join(self.results_path, "MSBooster", "MSBooster_plots", one_folder, file)))

        psm_ids = pd.DataFrame(columns=["Experiment", "PSM"])
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            psm_file = os.path.join(self.results_path, "psm.tsv")
            psm_lines = self.count_lines(psm_file, exp="One")
            psm_ids.loc[0] = {"Experiment": "One", "PSM": psm_lines}

        elif self.manifest_data["Bioreplicate"].isnull().any() and not self.manifest_data["Experiment"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                psm_file = os.path.join(self.results_path, str(exp), "psm.tsv")
                psm_lines = self.count_lines(psm_file, exp=str(exp))
                psm_ids.loc[count] = {"Experiment": str(exp), "PSM": psm_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any() and not self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                psm_file = os.path.join(self.results_path, "exp_" + str(bio_rep), "psm.tsv")
                psm_lines = self.count_lines(psm_file, exp=str(bio_rep))
                psm_ids.loc[count] = {"Experiment": str(bio_rep), "PSM": psm_lines}
                count += 1

        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                psm_file = os.path.join(self.results_path, str(exp) + "_" + str(bio), "psm.tsv")
                psm_lines = self.count_lines(psm_file, exp=str(exp) + "_" + str(bio))
                psm_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "PSM": psm_lines}
                count += 1

        return psm_ids

    def process_psm(self):
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            psm_file = os.path.join(self.results_path, "psm.tsv")
            psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
            psm_df["Retention"] = psm_df["Retention"] / 60
            psm_df["raw_file"] = psm_df["Spectrum"].apply(
                lambda x: os.path.basename(x).split(".")[0])
            for group, data in psm_df.groupby("raw_file"):
                self.single_run_data[group] = data

        elif self.manifest_data["Bioreplicate"].isnull().any():
            for exp in self.manifest_data["Experiment"].unique():
                psm_file = os.path.join(self.results_path, str(exp), "psm.tsv")
                psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
                psm_df["Retention"] = psm_df["Retention"] / 60
                psm_df["raw_file"] = psm_df["Spectrum"].apply(
                    lambda x: os.path.basename(x).split(".")[0])
                for group, data in psm_df.groupby("raw_file"):
                    self.single_run_data[group] = data
        elif self.manifest_data["Experiment"].isnull().any():
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                psm_file = os.path.join(self.results_path, "exp_" + str(bio_rep), "psm.tsv")
                psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
                psm_df["Retention"] = psm_df["Retention"] / 60
                psm_df["raw_file"] = psm_df["Spectrum"].apply(
                    lambda x: os.path.basename(x).split(".")[0])
                for group, data in psm_df.groupby("raw_file"):
                    self.single_run_data[group] = data

        else:
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                psm_file = os.path.join(self.results_path, str(exp) + "_" + str(bio), "psm.tsv")
                psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
                psm_df["Retention"] = psm_df["Retention"] / 60
                psm_df["raw_file"] = psm_df["Spectrum"].apply(
                    lambda x: os.path.basename(x).split(".")[0])
                for group, data in psm_df.groupby("raw_file"):
                    self.single_run_data[group] = data

    def read_peptides(self):
        # Read the Peptides data
        peptides_ids = pd.DataFrame(columns=["Experiment", "Peptides"])
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            peptides_file = os.path.join(self.results_path, "peptide.tsv")
            peptides_lines = self.count_lines(peptides_file)
            peptides_ids.loc[0] = {"Experiment": "One", "Peptides": peptides_lines}
        elif self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                peptides_file = os.path.join(self.results_path, str(exp), "peptide.tsv")
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(exp), "Peptides": peptides_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                peptides_file = os.path.join(self.results_path, "exp_" + str(bio_rep), "peptide.tsv")
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(bio_rep), "Peptides": peptides_lines}
                count += 1
        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                peptides_file = os.path.join(self.results_path, str(exp) + "_" + str(bio), "peptide.tsv")
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "Peptides": peptides_lines}
                count += 1
        return peptides_ids

    def read_proteins(self):
        # Read the Proteins data
        proteins_ids = pd.DataFrame(columns=["Experiment", "Proteins"])
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            proteins_file = os.path.join(self.results_path, "protein.tsv")
            proteins_lines = self.count_lines(proteins_file)
            proteins_ids.loc[0] = {"Experiment": "One", "Proteins": proteins_lines}
        elif self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                proteins_file = os.path.join(self.results_path, str(exp), "protein.tsv")
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(exp), "Proteins": proteins_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                proteins_file = os.path.join(self.results_path, "exp_" + str(bio_rep), "protein.tsv")
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(bio_rep), "Proteins": proteins_lines}
                count += 1
        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                proteins_file = os.path.join(self.results_path, str(exp) + "_" + str(bio), "protein.tsv")
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "Proteins": proteins_lines}
                count += 1
        return proteins_ids

    # Keep all your existing methods (read_data, get_running_time, etc.)
    # Just replace the plotting methods below

    def create_run_scatter_chart(self, data, title="Run Scatter Chart", x="Retention", y="Mass Error"):
        """Create scatter plot using matplotlib"""
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.scatter(data[x], data[y], alpha=0.5, s=10, color=NATURE_PALETTE[0])
        ax.set_xlabel(x)
        ax.set_ylabel(y)
        ax.set_title(title)
        ax.grid(True, alpha=0.3)
        return fig

    def create_run_distribution_plot(self, data, title="Run Distribution Chart", item=""):
        """Create distribution plot using matplotlib"""
        fig, ax = plt.subplots(figsize=(10, 6))
        create_distribution_plot(data, item, ax)
        ax.set_title(title)
        return fig

    def create_overview_table(self, data=None, title="Experiment design", plot_id="plot_table"):
        """Create table using matplotlib with improved layout"""
        if data is None:
            data = self.manifest_data

        # Calculate figure size based on number of rows - use letter width
        n_rows = len(data)
        fig_height = max(2, n_rows * 0.35 + 1.2)
        fig = plt.figure(figsize=(16, fig_height))
        ax = fig.add_subplot(111)
        ax.axis('tight')
        ax.axis('off')

        # Prepare table data
        table_data = []
        for idx, row in data.iterrows():
            table_data.append([str(val) for val in row.values])

        # Calculate column widths based on content
        col_widths = []
        for i, col in enumerate(data.columns):
            # Normalize widths (Spectrum File gets more space)
            if col == 'Spectrum File':
                col_widths.append(0.55)  # More space for file paths
            elif col == 'Run':
                col_widths.append(0.05)
            else:
                col_widths.append(0.13)

        # Create table
        table = ax.table(cellText=table_data,
                         colLabels=data.columns.tolist(),
                         cellLoc='left',
                         loc='center',
                         colWidths=col_widths)

        table.auto_set_font_size(False)
        table.set_fontsize(6)  # Default font size
        table.scale(1, 1.5)  # Row height

        # Style header with cyan background
        for i in range(len(data.columns)):
            cell = table[(0, i)]
            cell.set_facecolor('#A0E7E5')  # Cyan color like in image
            cell.set_text_props(weight='bold', color='black', fontsize=7)
            cell.set_edgecolor('white')
            cell.set_linewidth(2)

        # Style data rows - no alternating colors, white background
        # Style data rows - no alternating colors, white background
        for i in range(1, len(data) + 1):
            for j in range(len(data.columns)):
                cell = table[(i, j)]
                cell.set_facecolor('white')
                cell.set_edgecolor('#E0E0E0')
                cell.set_linewidth(1)

                # Adjust text padding - remove extra space at beginning
                cell.PAD = 0.05  # Reduce cell padding (default is 0.1)

                # For better left alignment
                if j > 0:  # For columns other than 'Run'
                    text_obj = cell.get_text()
                    text_obj.set_ha('left')  # Force horizontal alignment to left
                    text_obj.set_position((0.05, 0.5))  # Move text left (x=0.05 instead of default)

                # Check if the text is too long, especially in the Spectrum File column
                text = table_data[i - 1][j]
                spectrum_file_col_idx = data.columns.get_loc('Spectrum File') if 'Spectrum File' in data.columns else -1

                if j == spectrum_file_col_idx:  # For Spectrum File column
                    # Adjust font size based on text length
                    if len(text) > 100:
                        cell.get_text().set_fontsize(4)  # Very small font for very long text
                    elif len(text) > 70:
                        cell.get_text().set_fontsize(5)  # Small font for long text
                    # else keep default font size

        # Add title above table
        ax.text(0.02, 0.98, title, transform=ax.transAxes,
                fontsize=10, fontweight='bold', va='top')

        plt.subplots_adjust(left=0.02, right=0.98, top=0.95, bottom=0.02)

        return fig

    def create_gantt_chart(self, title="Workflow Processing Time"):
        """Create Gantt chart with actual timeline showing when each task ran"""
        if self.runtime_dict is None or len(self.runtime_dict) == 0:
            return None

        if self.finish_time is None:
            # Fallback to simple duration chart if no finish time
            return self._create_simple_duration_chart(title)

        fig, ax = plt.subplots(figsize=(16, max(6, len(self.runtime_dict) * 0.5)))

        # Calculate start and end times for each task (working backwards from finish_time)
        tasks = []
        current_end = self.finish_time

        # Reverse order to work backwards in time
        for task_name, duration in reversed(list(self.runtime_dict.items())):
            if isinstance(duration, (int, float)):
                task_start = current_end - dt.timedelta(minutes=duration)
                tasks.append({
                    'Task': task_name,
                    'Start': task_start,
                    'End': current_end,
                    'Duration': duration
                })
                current_end = task_start

        if not tasks:
            return None

        # Reverse to get chronological order
        tasks = list(reversed(tasks))
        df = pd.DataFrame(tasks)

        # Calculate total time
        total_time = sum(df['Duration'])  # In minutes

        # Determine the appropriate time interval based on total duration
        if total_time <= 10:  # Less than 10 minutes
            interval = 1  # 1 minute intervals
            time_fmt = '%H:%M'
        elif total_time <= 30:  # Less than 30 minutes
            interval = 5  # 5 minute intervals
            time_fmt = '%H:%M'
        elif total_time <= 120:  # Less than 2 hours
            interval = 15  # 15 minute intervals
            time_fmt = '%H:%M'
        elif total_time <= 360:  # Less than 6 hours
            interval = 30  # 30 minute intervals
            time_fmt = '%H:%M'
        else:  # More than 6 hours
            interval = 60  # 1 hour intervals
            time_fmt = '%H:%M'

        # Plot Gantt chart
        y_pos = np.arange(len(df))
        colors = ['#6B8EDB'] * len(df)  # Blue color like in the image

        for idx, row in df.iterrows():
            start_num = mdates.date2num(row['Start'])
            end_num = mdates.date2num(row['End'])
            duration_days = end_num - start_num

            # Draw bar
            ax.barh(idx, duration_days, left=start_num, height=0.6,
                    color=colors[idx], edgecolor='black', alpha=0.8, linewidth=0.5)

            # Add duration label on bar
            bar_center = start_num + duration_days / 2
            ax.text(bar_center, idx, f'{row["Duration"]:.2f} min',
                    ha='left', va='center', fontsize=8, fontweight='bold', color='black')

        # Format axes
        ax.set_yticks(y_pos)
        ax.set_yticklabels(df['Task'], fontsize=9)
        ax.set_xlabel('Time', fontsize=11)
        ax.set_ylabel('Task', fontsize=11)
        ax.set_title(title, fontsize=14, fontweight='bold', loc='left')

        # Format x-axis as time with appropriate interval
        ax.xaxis.set_major_formatter(mdates.DateFormatter(time_fmt))
        ax.xaxis.set_major_locator(mdates.MinuteLocator(interval=interval))
        plt.setp(ax.xaxis.get_majorticklabels(), rotation=0, ha='center')

        # Add total time annotation in top right
        ax.text(0.98, 0.98, f'Total Time: {total_time:.2f} min',
                transform=ax.transAxes, fontsize=10, fontweight='bold',
                verticalalignment='top', horizontalalignment='right',
                bbox=dict(boxstyle='round', facecolor='white', alpha=0.8, edgecolor='red'))

        # Grid
        ax.grid(True, alpha=0.3, axis='x')
        ax.set_axisbelow(True)

        # Invert y-axis to have first task at top
        ax.invert_yaxis()

        plt.tight_layout()
        return fig

    def _create_simple_duration_chart(self, title):
        """Fallback simple duration bar chart if no timeline available"""
        fig, ax = plt.subplots(figsize=(16, max(6, len(self.runtime_dict) * 0.5)))

        tasks = []
        for task_name, duration in self.runtime_dict.items():
            if isinstance(duration, (int, float)):
                tasks.append({'Task': task_name, 'Duration': duration})

        if not tasks:
            return None

        df = pd.DataFrame(tasks)
        df = df.sort_values('Duration', ascending=True)

        y_pos = np.arange(len(df))
        bars = ax.barh(y_pos, df['Duration'], color='#6B8EDB', edgecolor='black', alpha=0.7)

        for bar, duration in zip(bars, df['Duration']):
            ax.text(bar.get_width(), bar.get_y() + bar.get_height()/2,
                   f'{duration:.2f} min', ha='left', va='center', fontsize=9, fontweight='bold')

        ax.set_yticks(y_pos)
        ax.set_yticklabels(df['Task'])
        ax.set_xlabel('Duration (minutes)', fontsize=12)
        ax.set_title(title, fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3, axis='x')

        plt.tight_layout()
        return fig

    def create_exp_page3(self):
        """Create comprehensive overall statistics page matching plotly version layout"""
        fig = plt.figure(figsize=(16, 18))
        gs = fig.add_gridspec(3, 3, hspace=0.25, wspace=0.25,
                             width_ratios=[1, 2, 1],
                             left=0.08, right=0.97, top=0.95, bottom=0.05)
        fig.suptitle('Overall Statistics', fontsize=18, fontweight='bold', y=0.97)

        # Row 1, Col 1: Charge distribution (narrower)
        ax1 = fig.add_subplot(gs[0, 0])
        if not self.distribution_data.empty and 'Charge' in self.distribution_data.columns:
            charge_counts = self.distribution_data['Charge'].value_counts().sort_index()
            bars = ax1.bar(charge_counts.index.astype(str), charge_counts.values,
                          color='#696969', edgecolor='black', alpha=0.8, width=0.6)
            # Add value labels on bars
            for bar in bars:
                height = bar.get_height()
                ax1.text(bar.get_x() + bar.get_width()/2., height,
                        f'{int(height)}', ha='center', va='bottom', fontsize=9)
            ax1.set_xlabel('Charge States', fontsize=10)
            ax1.set_ylabel('# PSM', fontsize=10)
            ax1.set_title('Charge', fontsize=12, fontweight='bold')
            ax1.grid(True, alpha=0.3, axis='y')

        # Row 1, Col 2-3: Peptide Length distribution (spans 2 columns, wider)
        ax2 = fig.add_subplot(gs[0, 1:])
        if not self.distribution_data.empty and 'Peptide Length' in self.distribution_data.columns:
            ax2.hist(self.distribution_data['Peptide Length'].dropna(), bins=self.get_bin_size(self.distribution_data['Peptide Length'].dropna()),
                    color='#696969', edgecolor='black', alpha=0.8)
            ax2.set_xlabel('Peptide Length', fontsize=10)
            ax2.set_ylabel('# PSM', fontsize=10)
            ax2.set_title('Peptide Length', fontsize=12, fontweight='bold')
            ax2.grid(True, alpha=0.3, axis='y')

        # Row 2: PSM IDs, Peptide IDs, Protein IDs
        if self.id_nums is not None and not self.id_nums.empty:
            # Group and aggregate by experiment
            grouped = self.id_nums.groupby('Experiment').agg({
                'PSM': 'first' if 'PSM' in self.id_nums.columns else lambda x: 0,
                'Peptides': 'first' if 'Peptides' in self.id_nums.columns else lambda x: 0,
                'Proteins': 'first' if 'Proteins' in self.id_nums.columns else lambda x: 0
            }).reset_index()

            # PSM IDs
            ax3 = fig.add_subplot(gs[1, 0])
            if 'PSM' in grouped.columns:
                bars = ax3.bar(grouped['Experiment'], grouped['PSM'],
                              color='#696969', edgecolor='black', alpha=0.8)
                for bar in bars:
                    height = bar.get_height()
                    ax3.text(bar.get_x() + bar.get_width()/2., height/2,
                            str(int(height)), ha='center', va='center',
                            fontsize=12, fontweight='bold', color='white')
                ax3.set_ylabel('# PSM', fontsize=10)
                ax3.set_xlabel('Experiments', fontsize=10)
                ax3.set_title('PSM IDs', fontsize=12, fontweight='bold')

            # Peptide IDs
            ax4 = fig.add_subplot(gs[1, 1])
            if 'Peptides' in grouped.columns:
                bars = ax4.bar(grouped['Experiment'], grouped['Peptides'],
                              color='#696969', edgecolor='black', alpha=0.8)
                for bar in bars:
                    height = bar.get_height()
                    ax4.text(bar.get_x() + bar.get_width()/2., height/2,
                            str(int(height)), ha='center', va='center',
                            fontsize=12, fontweight='bold', color='white')
                ax4.set_ylabel('# Peptides', fontsize=10)
                ax4.set_xlabel('Experiments', fontsize=10)
                ax4.set_title('Peptide IDs', fontsize=12, fontweight='bold')

            # Protein IDs
            ax5 = fig.add_subplot(gs[1, 2])
            if 'Proteins' in grouped.columns:
                bars = ax5.bar(grouped['Experiment'], grouped['Proteins'],
                              color='#696969', edgecolor='black', alpha=0.8)
                for bar in bars:
                    height = bar.get_height()
                    ax5.text(bar.get_x() + bar.get_width()/2., height/2,
                            str(int(height)), ha='center', va='center',
                            fontsize=12, fontweight='bold', color='white')
                ax5.set_ylabel('# Proteins', fontsize=10)
                ax5.set_xlabel('Experiments', fontsize=10)
                ax5.set_title('Protein IDs', fontsize=12, fontweight='bold')

        # Row 3, Col 1: Missed Cleavage
        ax6 = fig.add_subplot(gs[2, 0])
        if not self.distribution_data.empty and 'Number of Missed Cleavages' in self.distribution_data.columns:
            mc_counts = self.distribution_data['Number of Missed Cleavages'].value_counts().sort_index()
            bars = ax6.bar(mc_counts.index.astype(str), mc_counts.values,
                          color='#696969', edgecolor='black', alpha=0.8, width=0.6)
            for bar in bars:
                height = bar.get_height()
                ax6.text(bar.get_x() + bar.get_width()/2., height,
                        f'{int(height)}', ha='center', va='bottom', fontsize=9)
            ax6.set_xlabel('Missed Cleavages', fontsize=10)
            ax6.set_ylabel('# PSM', fontsize=10)
            ax6.set_title('Missed Cleavage', fontsize=12, fontweight='bold')
            ax6.grid(True, alpha=0.3, axis='y')

        # Row 3, Col 2: MS1 Mass Error
        ax7 = fig.add_subplot(gs[2, 1])
        if self.ms1_mass_df is not None and not self.ms1_mass_df.empty and 'Calib' in self.ms1_mass_df.columns:
            runs = sorted(self.ms1_mass_df['Run'].unique())
            x = np.arange(len(runs))
            width = 0.35

            old_data = self.ms1_mass_df[self.ms1_mass_df['Calib'] == 'Old'].sort_values('Run')
            new_data = self.ms1_mass_df[self.ms1_mass_df['Calib'] == 'Calibrated'].sort_values('Run')

            # Calculate shared y-axis range
            ms1_min, ms1_max = self.ms1_mass_df['Value'].min(), self.ms1_mass_df['Value'].max()
            ms2_min, ms2_max = (self.ms2_mass_df['Value'].min(), self.ms2_mass_df['Value'].max()) if self.ms2_mass_df is not None and not self.ms2_mass_df.empty else (ms1_min, ms1_max)
            all_min = min(ms1_min, ms2_min) * 1.2
            all_max = max(ms1_max, ms2_max) * 1.2

            ax7.bar(x - width/2, old_data['Value'].values, width,
                   color='#696969', edgecolor='black', alpha=0.8, label='Before (Grey)')
            ax7.bar(x + width/2, new_data['Value'].values, width,
                   color='#000000', edgecolor='black', alpha=0.8, label='After Calibration (Black)')

            ax7.set_xlabel('Run', fontsize=10)
            ax7.set_ylabel('MS1 Mass Error (Median)', fontsize=10)
            ax7.set_title('MS1 Mass Error\nBefore (Grey) vs After Calibration (Black)', fontsize=11, fontweight='bold')
            ax7.set_xticks(x)
            ax7.set_xticklabels([str(r) for r in runs], fontsize=8)
            ax7.set_ylim(all_min, all_max)
            ax7.grid(True, alpha=0.3, axis='y')
            ax7.axhline(y=0, color='black', linestyle='-', linewidth=0.8)

        # Row 3, Col 3: MS2 Mass Error
        ax8 = fig.add_subplot(gs[2, 2])
        if self.ms2_mass_df is not None and not self.ms2_mass_df.empty and 'Calib' in self.ms2_mass_df.columns:
            runs = sorted(self.ms2_mass_df['Run'].unique())
            x = np.arange(len(runs))
            width = 0.35

            old_data = self.ms2_mass_df[self.ms2_mass_df['Calib'] == 'Old'].sort_values('Run')
            new_data = self.ms2_mass_df[self.ms2_mass_df['Calib'] == 'Calibrated'].sort_values('Run')

            ax8.bar(x - width/2, old_data['Value'].values, width,
                   color='#696969', edgecolor='black', alpha=0.8, label='Before (Grey)')
            ax8.bar(x + width/2, new_data['Value'].values, width,
                   color='#000000', edgecolor='black', alpha=0.8, label='After Calibration (Black)')

            ax8.set_xlabel('Run', fontsize=10)
            ax8.set_ylabel('MS2 Mass Error (Median)', fontsize=10)
            ax8.set_title('MS2 Mass Error\nBefore (Grey) vs After Calibration (Black)', fontsize=11, fontweight='bold')
            ax8.set_xticks(x)
            ax8.set_xticklabels([str(r) for r in runs], fontsize=8)
            ax8.set_ylim(all_min, all_max)
            ax8.grid(True, alpha=0.3, axis='y')
            ax8.axhline(y=0, color='black', linestyle='-', linewidth=0.8)

        return fig

    def make_composite(self, run_name, images):
        """Create composite page with feature weights plot next to first image, rest stacked below"""

        # Check if we have feature weights for this run
        if run_name not in self.features_weight:
            # Fallback: just stack the MSBooster images
            max_width = max(im.width for im in images)
            total_height = sum(im.height for im in images)

            all_composite = Image.new("RGB", (max_width, total_height), "white")
            current_y = 0
            for im in images:
                all_composite.paste(im, (0, current_y))
                current_y += im.height

            padded = ImageOps.expand(all_composite, border=20, fill="white")
            png_buffer = BytesIO()
            padded.save(png_buffer, format="PDF")
            png_buffer.seek(0)
            return png_buffer

        # Create Percolator feature weights plot
        features_fig = self.create_percolator_features_plot(run_name)
        if features_fig is None:
            # Fallback if plot creation fails
            max_width = max(im.width for im in images)
            total_height = sum(im.height for im in images)
            all_composite = Image.new("RGB", (max_width, total_height), "white")
            current_y = 0
            for im in images:
                all_composite.paste(im, (0, current_y))
                current_y += im.height
            padded = ImageOps.expand(all_composite, border=20, fill="white")
            png_buffer = BytesIO()
            padded.save(png_buffer, format="PDF")
            png_buffer.seek(0)
            return png_buffer

        # Convert plot to image
        buf_plot = fig_to_png_bytes(features_fig)
        plot_img = Image.open(buf_plot)

        if len(images) == 0:
            # Only plot, no images
            final_composite = plot_img
        else:
            # Row 1: Plot + First Image side by side
            first_img = images[0]

            # Resize plot to match first image height
            plot_aspect = plot_img.width / plot_img.height
            new_plot_height = first_img.height
            new_plot_width = int(new_plot_height * plot_aspect)
            plot_img_resized = plot_img.resize((new_plot_width, new_plot_height), Image.LANCZOS)

            # Create first row composite
            row1_width = new_plot_width + first_img.width
            row1_composite = Image.new("RGB", (row1_width, first_img.height), "white")
            row1_composite.paste(plot_img_resized, (0, 0))
            row1_composite.paste(first_img, (new_plot_width, 0))

            # Stack remaining images below
            all_rows = [row1_composite]
            for img in images[1:]:
                all_rows.append(img)

            # Calculate final dimensions
            max_width = max(row.width for row in all_rows)
            total_height = sum(row.height for row in all_rows)

            # Create final composite
            final_composite = Image.new("RGB", (max_width, total_height), "white")
            current_y = 0
            for row in all_rows:
                # Center each row if it's narrower than max_width
                x_offset = (max_width - row.width) // 2
                final_composite.paste(row, (x_offset, current_y))
                current_y += row.height

        # Add border and convert to PDF
        padded = ImageOps.expand(final_composite, border=20, fill="white")
        png_buffer = BytesIO()
        padded.save(png_buffer, format="PDF")
        png_buffer.seek(0)

        return png_buffer

    def create_percolator_features_plot(self, exp_name):
        """Create percolator features weight plot"""
        if exp_name not in self.features_weight:
            return None

        df = self.features_weight[exp_name]

        fig, ax = plt.subplots(figsize=(10, max(6, len(df) * 0.4)))

        # Create horizontal bar plot
        y_pos = np.arange(len(df))
        colors = ['green' if x > 0 else 'red' for x in df['Mean Weight']]

        bars = ax.barh(y_pos, df['Mean Weight'], color=colors, alpha=0.7, edgecolor='black')

        # Calculate max absolute value for setting x-axis limits
        max_abs_weight = df['Mean Weight'].abs().max()
        x_margin = max_abs_weight * 0.25  # 25% margin for labels

        # Add value labels on bars
        for i, (bar, weight) in enumerate(zip(bars, df['Mean Weight'])):
            x_pos = bar.get_width()
            # Position text at the end of the bar
            h_align = 'left' if weight > 0 else 'right'
            x_offset = max_abs_weight * 0.02 if weight > 0 else -max_abs_weight * 0.02
            ax.text(x_pos + x_offset, bar.get_y() + bar.get_height()/2,
                    f'{weight:.4f}', ha=h_align, va='center', fontsize=8, fontweight='bold')

        ax.set_yticks(y_pos)
        ax.set_yticklabels(df['FeatureName'], fontsize=9)
        ax.set_xlabel('Mean Weight', fontsize=10)
        ax.set_title(f'Percolator Feature Weights - {exp_name}', fontsize=11, fontweight='bold')
        ax.axvline(x=0, color='black', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.3, axis='x')

        # Set x-axis limits to accommodate labels
        ax.set_xlim(-max_abs_weight - x_margin, max_abs_weight + x_margin)

        plt.tight_layout()
        return fig

    def create_mass_calibration_plot(self):
        """Create mass calibration before/after plot"""
        if self.ms1_mass_df is None or self.ms2_mass_df is None:
            return None, None

        # MS1 plot
        fig1, ax1 = plt.subplots(figsize=(10, 6))
        if not self.ms1_mass_df.empty:
            create_mass_error_plot(self.ms1_mass_df, ax1)
            ax1.set_title(f'MS1 Mass Error (Tolerance: {self.ms1_tolerance} {self.ms1_units})')
            ax1.set_ylabel(f'Mass Error ({self.ms1_units})')

        # MS2 plot
        fig2, ax2 = plt.subplots(figsize=(10, 6))
        if not self.ms2_mass_df.empty:
            create_mass_error_plot(self.ms2_mass_df, ax2)
            ax2.set_title(f'MS2 Mass Error (Tolerance: {self.ms2_tolerance} {self.ms2_units})')
            ax2.set_ylabel(f'Mass Error ({self.ms2_units})')

        return fig1, fig2

    def create_id_numbers_plot(self):
        """Create PSM/Peptide/Protein ID counts plot"""
        if self.id_nums is None or self.id_nums.empty:
            return None

        # Group by experiment and get PSM, Peptides, Proteins
        grouped = self.id_nums.groupby('Experiment').first().reset_index()

        fig, ax = plt.subplots(figsize=(12, 6))

        x = np.arange(len(grouped))
        width = 0.25

        # Plot bars for each metric
        if 'PSM' in grouped.columns:
            ax.bar(x - width, grouped['PSM'], width, label='PSM',
                   color='#808080', edgecolor='black', alpha=0.7)
        if 'Peptides' in grouped.columns:
            ax.bar(x, grouped['Peptides'], width, label='Peptides',
                   color='#A9A9A9', edgecolor='black', alpha=0.7)
        if 'Proteins' in grouped.columns:
            ax.bar(x + width, grouped['Proteins'], width, label='Proteins',
                   color='#C0C0C0', edgecolor='black', alpha=0.7)

        ax.set_xlabel('Experiment')
        ax.set_ylabel('Count')
        ax.set_title('Identification Numbers by Experiment')
        ax.set_xticks(x)
        ax.set_xticklabels(grouped['Experiment'], rotation=45, ha='right')
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')

        plt.tight_layout()
        return fig

    def get_bin_size(self, processed_data):

        # Get min and max values
        min_length = processed_data.min()
        max_length = processed_data.max()
        range_length = max_length - min_length

        # Determine appropriate bin size:
        # For small ranges, use integer bins (one bin per integer value)
        # For larger ranges, use adaptive binning based on data distribution
        if range_length <= 20:
            # Create integer bins when range is small
            bins = np.arange(min_length, max_length + 2) - 0.5  # +2 to include the last value and offset by 0.5
        else:
            # Freedman-Diaconis rule for bin width when range is larger
            # This rule works well for various distributions
            q75, q25 = np.percentile(processed_data, [75, 25])
            iqr = q75 - q25
            bin_width = 2 * iqr * (len(processed_data) ** (-1 / 3))
            bin_width = max(1, round(bin_width))  # At least 1, and round to nearest integer
            bins = np.arange(min_length, max_length + bin_width + 1, bin_width) - 0.5

        return bins

    def create_run_page(self, run_name):
        """Create individual run page with 3x3 plot grid"""
        if run_name not in self.single_run_data:
            return None

        data = self.single_run_data[run_name]

        fig, axes = plt.subplots(3, 3, figsize=(16, 18))
        fig.suptitle(f'{run_name} Statistics', fontsize=16, fontweight='bold')

        # Row 1, Col 1: Peptide Length distribution
        if 'Peptide Length' in data.columns:
            axes[0, 0].hist(data['Peptide Length'].dropna(), bins=self.get_bin_size(data['Peptide Length'].dropna()),
                            color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[0, 0].set_xlabel('Peptide Length')
            axes[0, 0].set_ylabel('# PSM')
            axes[0, 0].set_title('Peptide Length Distribution')
            axes[0, 0].grid(True, alpha=0.3)

        # Row 1, Col 2: Missed Cleavages
        if 'Number of Missed Cleavages' in data.columns:
            mc_counts = data['Number of Missed Cleavages'].value_counts().sort_index()
            axes[0, 1].bar(mc_counts.index.astype(str), mc_counts.values,
                           color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[0, 1].set_xlabel('Number of Missed Cleavages')
            axes[0, 1].set_ylabel('# PSM')
            axes[0, 1].set_title('Missed Cleavages Distribution')
            axes[0, 1].grid(True, alpha=0.3)

        # Row 1, Col 3: Charge
        if 'Charge' in data.columns:
            charge_counts = data['Charge'].value_counts().sort_index()
            axes[0, 2].bar(charge_counts.index.astype(str), charge_counts.values,
                           color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[0, 2].set_xlabel('Charge')
            axes[0, 2].set_ylabel('# PSM')
            axes[0, 2].set_title('Charge Distribution')
            axes[0, 2].grid(True, alpha=0.3)

        # Row 2, Col 1: M/Z distribution
        if 'Calculated M/Z' in data.columns:
            axes[1, 0].hist(data['Calculated M/Z'].dropna(), bins=50,
                            color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[1, 0].set_xlabel('M/Z')
            axes[1, 0].set_ylabel('# PSM')
            axes[1, 0].set_title('M/Z Distribution')
            axes[1, 0].grid(True, alpha=0.3)

        # Row 2, Col 2: Retention Time distribution
        if 'Retention' in data.columns:
            axes[1, 1].hist(data['Retention'].dropna(), bins=50,
                            color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[1, 1].set_xlabel('Retention Time (min)')
            axes[1, 1].set_ylabel('# PSM')
            axes[1, 1].set_title('Retention Time Distribution')
            axes[1, 1].grid(True, alpha=0.3)

        # Row 2, Col 3: Hyperscore distribution
        if 'Hyperscore' in data.columns:
            axes[1, 2].hist(data['Hyperscore'].dropna(), bins=50,
                            color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
            axes[1, 2].set_xlabel('Hyperscore')
            axes[1, 2].set_ylabel('# PSM')
            axes[1, 2].set_title('Hyperscore Distribution')
            axes[1, 2].grid(True, alpha=0.3)

        # Row 3, Col 1: Delta Mass distribution
        if 'Delta Mass' in data.columns:
            delta_mass = data['Delta Mass'].dropna()

            # Calculate range of delta mass values
            delta_range = delta_mass.max() - delta_mass.min()

            if delta_range < 6:
                # Round to integers when range is small
                delta_mass_rounded = delta_mass.round().astype(int)

                # Count occurrences of each integer value
                value_counts = delta_mass_rounded.value_counts().sort_index()

                # Create bar chart with integer x-axis
                axes[2, 0].bar(value_counts.index, value_counts.values,
                               color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)

                # Ensure x-axis shows only integer ticks
                axes[2, 0].set_xticks(sorted(value_counts.index))
            else:
                # Use regular histogram for larger ranges
                filtered_delta_mass = delta_mass[~delta_mass.between(-1.5, 3)]

                # Only proceed if we have data left after filtering
                if not filtered_delta_mass.empty:
                    axes[2, 0].hist(filtered_delta_mass,
                                    bins=self.get_bin_size(filtered_delta_mass),
                                    color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)
                else:
                    # Fallback to using all data if filtering removes everything
                    axes[2, 0].hist(delta_mass,
                                    bins=self.get_bin_size(delta_mass),
                                    color=NATURE_PALETTE[0], edgecolor='black', alpha=0.7)

            # Set proper labels and styling
            axes[2, 0].set_xlabel('Delta Mass (Da)')
            axes[2, 0].set_ylabel('# PSM')
            axes[2, 0].set_title('Delta Mass Dis')
            axes[2, 1].grid(True, alpha=0.3)

        # Row 3, Col 2: M/Z vs Delta Mass scatter
        if 'Calculated M/Z' in data.columns and 'Delta Mass' in data.columns:
            filtered_data = data[data['Delta Mass'].abs() < 0.5]
            axes[2, 1].scatter(filtered_data['Calculated M/Z'], filtered_data['Delta Mass'],
                               alpha=0.8, s=7, color=NATURE_PALETTE[0], rasterized=True)
            axes[2, 1].set_xlabel('M/Z')
            axes[2, 1].set_ylabel('Delta Mass (Da)')
            axes[2, 1].set_title('M/Z vs Delta Mass')
            axes[2, 1].grid(True, alpha=0.3)

        # Row 3, Col 3: Retention Time vs M/Z scatter
        if 'Retention' in data.columns and 'Calculated M/Z' in data.columns:
            axes[2, 2].scatter(data['Retention'], data['Calculated M/Z'],
                               alpha=0.8, s=7, color=NATURE_PALETTE[0], rasterized=True)
            axes[2, 2].set_xlabel('Retention Time (min)')
            axes[2, 2].set_ylabel('M/Z')
            axes[2, 2].set_title('Retention Time vs M/Z')
            axes[2, 2].grid(True, alpha=0.3)

        plt.tight_layout()
        return fig

    # Keep all your other existing methods (read_data, get_mass_error, etc.)


def main():
    parser = argparse.ArgumentParser(description="FragPipe report generator (Matplotlib version)")
    parser.add_argument("-r", "--results_path", type=str, required=True)
    args = parser.parse_args()
    results_path = args.results_path

    pdf_pages = []
    fragPipeReport = FragPipeReport(results_path=results_path)

    running_table_fig = fragPipeReport.create_overview_table()
    running_time_fig = fragPipeReport.create_gantt_chart()

    # Convert to PDF
    # Page 1: Overview table
    if running_table_fig:
        page1_pdf = fig_to_pdf_bytes(running_table_fig)
        pdf_pages.append(page1_pdf)

    # Page 2: Gantt chart
    if running_time_fig:
        page2_pdf = fig_to_pdf_bytes(running_time_fig)
        pdf_pages.append(page2_pdf)

    # Page 3: ID numbers plot
    overall_stats_fig = fragPipeReport.create_exp_page3()
    if overall_stats_fig:
        page3_pdf = fig_to_pdf_bytes(overall_stats_fig)
        pdf_pages.append(page3_pdf)


    # Pages: Individual run pages
    if fragPipeReport.single_run_data:
        for run_name in fragPipeReport.single_run_data.keys():
            run_fig = fragPipeReport.create_run_page(run_name)
            if run_fig:
                pdf_pages.append(fig_to_pdf_bytes(run_fig))

        for run_name in fragPipeReport.single_run_data.keys():    # Add MSBooster plots if available
            if run_name in fragPipeReport.msbooster_plots:
                composite_pdf = fragPipeReport.make_composite(run_name, fragPipeReport.msbooster_plots[run_name])
                if composite_pdf:
                    pdf_pages.append(composite_pdf)

    # Merge all PDFs
    if pdf_pages:
        final_pdf = merge_pdf_buffers(pdf_pages)
        output_path = os.path.join(results_path, "fragpipe-report.pdf")
        with open(output_path, 'wb') as f:
            f.write(final_pdf.read())
        print(f"✓ Report generated successfully: {output_path}")
        print(f"✓ Total pages: {len(pdf_pages)}")
    else:
        print("No data to generate report")
        print("⚠ Warning: No plots were generated. Please check your input data.")


if __name__ == '__main__':
    main()