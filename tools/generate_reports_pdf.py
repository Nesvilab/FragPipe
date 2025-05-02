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
import math
import argparse

import datetime as dt
import pandas as pd
import plotly.graph_objs as go
import plotly.io as pio
import plotly.express as px

from plotly.subplots import make_subplots
from io import BytesIO
from PyPDF2 import PdfMerger
from PIL import Image, ImageOps


NATURE_PALETTE = ["grey", "grey"]
DEFAULT_TEMPLATE = "simple_white"


def compute_auto_width(series, min_width=50, char_width=10):
    # Compute a width based on the maximum character count; adjust parameters as needed.
    max_len = series.astype(str).str.len().max()
    return max(min_width, max_len * char_width)


def create_distribution_plot(data=None, item=""):
    # Sort the data by the specified column
    data = data.sort_values(by=item)
    # if plot_id != "plot_Peptide Length":
    #     data[item] = data[item].astype(str)
    bins_num = 50
    if item != "Peptide Length":
        bins_num = 10
    if item == "Charge" or item == "Number of Missed Cleavages":
        data[item] = data[item].astype(str)

    fig = go.Histogram(
        x=data[item],
        # color="Exp",
        histnorm=None,
        nbinsx=bins_num,
    )

    return fig


def create_bar_chart(data=None, x="", y=""):
    fig = go.Bar(x=data[x].astype(str),
                 y=data[y],
                 text=data[y],
                 textposition='auto',
                 textfont_color="black",
                 )

    return fig


def create_mass_error_plot(data):
    fig = go.Figure()

    for exp_name, grp in data.groupby("Calib"):
        fig.add_trace(go.Bar(
            x=grp["Run"],
            y=grp["Value"],
        ))

    fig.update_layout(
        title="Grouped Bar Plot",
        xaxis_title="Fruit",
        yaxis_title="Quantity",
        barmode="group",  # side‑by‑side bars
        colorway=["darkgrey", "grey"],
        template="simple_white",
    )

    return fig


def create_exp_page_running(all_figs, run_name):
    whole_fig = make_subplots(
        rows=3, cols=3,
        # For row 1, we will use col1 and col3; we leave col2 empty.
        specs=[
            [{}, {}, {}],
            [{}, {}, {}],
            [{}, {}, {}]
        ],
        subplot_titles=[
            "Peptide Length/# PSM", "Missed Cleavage/# PSM", "Charge/# PSM",
            # Row 1: we leave the middle title empty
            "MZ/# PSM", "RT/# PSM", "Hyperscore/# PSM",  # Row 2
            "Delta Mass Dis", "MZ/Delta Mass", "RT/MZ"  # Row 3
        ],
        vertical_spacing=0.05,  # Reduced vertical spacing
        horizontal_spacing=0.05  # Reduced horizontal spacing
    )

    row = 1
    col = 1
    for fig in all_figs:
        if row > 3:
            break
        whole_fig.add_trace(fig, row=row, col=col)
        if col == 3:
            col = 1
            row += 1
        else:
            col += 1

    # --- Update axis labels for each XY subplot ---
    # Row 1:
    whole_fig.update_xaxes(title_text="Peptide Length", row=1, col=1)
    whole_fig.update_yaxes(title_text="# PSM", row=1, col=1)
    whole_fig.update_xaxes(title_text="Missed Cleavage", row=1, col=2)
    whole_fig.update_yaxes(title_text="# PSM", row=1, col=2)
    whole_fig.update_xaxes(title_text="Charge", row=1, col=3)
    whole_fig.update_yaxes(title_text="# PSM", row=1, col=3)

    whole_fig.update_xaxes(title_text="M/Z", row=2, col=1)
    whole_fig.update_yaxes(title_text="# PSM", row=2, col=1)
    whole_fig.update_xaxes(title_text="Retention Time", row=2, col=2)
    whole_fig.update_yaxes(title_text="# PSM", row=2, col=2)
    whole_fig.update_xaxes(title_text="Hyperscore", row=2, col=3)
    whole_fig.update_yaxes(title_text="# PSM", row=2, col=3)

    # Row 3:
    whole_fig.update_xaxes(title_text="Delta Mass (Da)", row=3, col=1)
    whole_fig.update_yaxes(title_text="# PSM", row=3, col=1)
    whole_fig.update_xaxes(title_text="M/Z", row=3, col=2)
    whole_fig.update_yaxes(title_text="Delta Mass (Da)", row=3, col=2)
    whole_fig.update_xaxes(title_text="Retention Time", row=3, col=3)
    whole_fig.update_yaxes(title_text="M/Z", row=3, col=3)

    whole_fig.update_layout(
        title_text=run_name + " Statistics",
        width=1600,  # final PDF width in pixels
        height=1800,  # final PDF height in pixels
        showlegend=False,
        template=DEFAULT_TEMPLATE,
        colorway=NATURE_PALETTE,
        bargap=0.1,
    )

    return whole_fig


def fig_to_pdf_bytes(fig):
    pdf_buffer = BytesIO()

    fig.write_image(pdf_buffer, format="pdf")
    pdf_buffer.seek(0)
    # Optional: set a dummy name attribute
    return pdf_buffer


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
        self.exp_plots = []  # List to store tuple of (div_html, plot_id, plot_title)
        self.session2_plots = {}  # Session 2 (run-specific) charts
        self.run_data = {}  # Will be set externally in main()
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
                self.manifest_data = pd.read_csv(self.results_path + file, sep="\t", header=None,
                                                 dtype=str)
            if file == "fragpipe.workflow":
                self.workflow_file = self.results_path + file
            if file.startswith("log_"):
                log_files.append(file)

        log_files.sort()
        self.latest_log_file = log_files[-1]

        with open(self.results_path + self.latest_log_file, 'r') as f:
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
        with open(self.results_path + self.latest_log_file, "r") as f:
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

        # 2) convert to datetime.time objects
        td_list = [
            dt.timedelta(hours=int(h), minutes=int(m), seconds=int(s))
            for h, m, s in times
        ]

        creation_timestamp = os.path.getctime(self.results_path + self.latest_log_file)
        creation_datetime = dt.datetime.fromtimestamp(creation_timestamp)

        self.finish_time = dt.datetime.combine(creation_datetime.date(), dt.time()) + max(td_list)

    def get_running_time(self):
        self.runtime_dict = {}
        fragger_time = {}
        running_time_region = False
        main_search_region = False
        with open(self.results_path + self.latest_log_file, 'r') as f:
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
        with open(self.results_path + self.latest_log_file, 'r') as f:
            log_lines = f.readlines()
        for line in log_lines:

            if line.startswith("*********************MASS CALIBRATION AND PARAMETER OPTIMIZATION*******************") or \
                    line.startswith("*********************************MASS CALIBRATION**"):
                mass_error_region = True
            if mass_error_region:
                line_stripped = line.strip()
                # Match lines that start with one or more digits (like "001")
                if re.match(r"^\d+", line_stripped):
                    data_rows.append(line_stripped)
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
            run = int(parts[0].strip())

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

        if os.path.exists(self.results_path + "\\MSBooster\\MSBooster_plots"):
            msbooster_files = os.listdir(self.results_path + "\\MSBooster\\MSBooster_plots")
            for one_folder in msbooster_files:
                # check if the folder is a directory
                if not os.path.isdir(self.results_path + "\\MSBooster\\MSBooster_plots\\" + one_folder):
                    continue
                for file in os.listdir(self.results_path + "\\MSBooster\\MSBooster_plots\\" + one_folder):
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
                                    self.results_path + "\\MSBooster\\MSBooster_plots\\" + one_folder + "\\" + file))
                        if "delta_RT_loess" in file or "pred_RT_real_units" in file or "unweighted_spectral_entropy" in file:
                            self.msbooster_plots[run_name].append(
                                Image.open(
                                    self.results_path + "\\MSBooster\\MSBooster_plots\\" + one_folder + "\\" + file))

        psm_ids = pd.DataFrame(columns=["Experiment", "PSM"])
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            psm_file = self.results_path + "\\psm.tsv"
            psm_lines = self.count_lines(psm_file, exp="One")
            psm_ids.loc[0] = {"Experiment": "One", "PSM": psm_lines}

        elif self.manifest_data["Bioreplicate"].isnull().any() and not self.manifest_data["Experiment"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                psm_file = self.results_path + "\\" + str(exp) + "\\psm.tsv"
                psm_lines = self.count_lines(psm_file, exp=str(exp))
                psm_ids.loc[count] = {"Experiment": str(exp), "PSM": psm_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any() and not self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                psm_file = self.results_path + "\\exp_" + str(bio_rep) + "\\psm.tsv"
                psm_lines = self.count_lines(psm_file, exp=str(bio_rep))
                psm_ids.loc[count] = {"Experiment": str(bio_rep), "PSM": psm_lines}
                count += 1

        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                psm_file = self.results_path + "\\" + str(exp) + "_" + str(bio) + "\\psm.tsv"
                psm_lines = self.count_lines(psm_file, exp=str(exp) + "_" + str(bio))
                psm_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "PSM": psm_lines}
                count += 1

        return psm_ids

    def process_psm(self):
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            psm_file = self.results_path + "\\psm.tsv"
            psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
            psm_df["Retention"] = psm_df["Retention"] / 60
            psm_df["raw_file"] = psm_df["Spectrum"].apply(
                lambda x: os.path.basename(x).split(".")[0])
            for group, data in psm_df.groupby("raw_file"):
                self.single_run_data[group] = data

        elif self.manifest_data["Bioreplicate"].isnull().any():
            for exp in self.manifest_data["Experiment"].unique():
                psm_file = self.results_path + "\\" + str(exp) + "\\psm.tsv"
                psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
                psm_df["Retention"] = psm_df["Retention"] / 60
                psm_df["raw_file"] = psm_df["Spectrum"].apply(
                    lambda x: os.path.basename(x).split(".")[0])
                for group, data in psm_df.groupby("raw_file"):
                    self.single_run_data[group] = data
        elif self.manifest_data["Experiment"].isnull().any():
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                psm_file = self.results_path + "\\exp_" + str(bio_rep) + "\\psm.tsv"
                psm_df = pd.read_csv(psm_file, sep="\t", on_bad_lines="skip", engine='pyarrow')
                psm_df["Retention"] = psm_df["Retention"] / 60
                psm_df["raw_file"] = psm_df["Spectrum"].apply(
                    lambda x: os.path.basename(x).split(".")[0])
                for group, data in psm_df.groupby("raw_file"):
                    self.single_run_data[group] = data

        else:
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                psm_file = self.results_path + "\\" + str(exp) + "_" + str(bio) + "\\psm.tsv"
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
            peptides_file = self.results_path + "\\peptide.tsv"
            peptides_lines = self.count_lines(peptides_file)
            peptides_ids.loc[0] = {"Experiment": "One", "Peptides": peptides_lines}
        elif self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                peptides_file = self.results_path + "\\" + str(exp) + "\\peptide.tsv"
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(exp), "Peptides": peptides_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                peptides_file = self.results_path + "\\exp_" + str(bio_rep) + "\\peptide.tsv"
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(bio_rep), "Peptides": peptides_lines}
                count += 1
        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                peptides_file = self.results_path + "\\" + str(exp) + "_" + str(bio) + "\\peptide.tsv"
                peptides_lines = self.count_lines(peptides_file)
                peptides_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "Peptides": peptides_lines}
                count += 1
        return peptides_ids

    def read_proteins(self):
        # Read the Proteins data
        proteins_ids = pd.DataFrame(columns=["Experiment", "Proteins"])
        if (self.manifest_data["Experiment"].isnull().any() and self.manifest_data["Bioreplicate"].isnull().any()) or self.run_spec_lib:
            proteins_file = self.results_path + "\\protein.tsv"
            proteins_lines = self.count_lines(proteins_file)
            proteins_ids.loc[0] = {"Experiment": "One", "Proteins": proteins_lines}
        elif self.manifest_data["Bioreplicate"].isnull().any():
            count = 0
            for exp in self.manifest_data["Experiment"].unique():
                proteins_file = self.results_path + "\\" + str(exp) + "\\protein.tsv"
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(exp), "Proteins": proteins_lines}
                count += 1
        elif self.manifest_data["Experiment"].isnull().any():
            count = 0
            for bio_rep in self.manifest_data["Bioreplicate"].unique():
                proteins_file = self.results_path + "\\exp_" + str(bio_rep) + "\\protein.tsv"
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(bio_rep), "Proteins": proteins_lines}
                count += 1
        else:
            count = 0
            for exp, bio in zip(self.manifest_data["Experiment"], self.manifest_data["Bioreplicate"]):
                proteins_file = self.results_path + "\\" + str(exp) + "_" + str(bio) + "\\protein.tsv"
                proteins_lines = self.count_lines(proteins_file)
                proteins_ids.loc[count] = {"Experiment": str(exp) + "_" + str(bio), "Proteins": proteins_lines}
                count += 1
        return proteins_ids

    def create_run_scatter_chart(self, data, title="Run Scatter Chart", x="Retention",
                                 y="Mass Error"):

        if y == "Delta Mass":
            data = data[data[y].abs() < 0.5]
        fig = go.Scattergl(x=data[x], y=data[y], mode='markers')

        return fig

    def create_run_distribution_plot(self, data, title="Run Distribution Chart",
                                     item=""):
        nbins = 50
        if item == "Delta Mass":
            vals = data[item].dropna()
            vmin, vmax = vals.min(), vals.max()
            vrange = vmax - vmin

            if vrange < 6:
                data["IntDelta"] = data[item].round(0) # round to integer
                delta_ids =data.groupby('IntDelta').size().reset_index(name='Count')
                # integer bins
                fig = go.Bar(x=delta_ids["IntDelta"].astype(str),
                             y=delta_ids['Count'],
                             )
            else:
                # finer bins: fixed number
                nbins = math.ceil(vrange/2)
                # filter data[item] between -1.5 and 3
                filtered_data = data[~data[item].between(-1.5, 3)]

                filtered_data_vals = filtered_data[item].dropna()
                hist_kwargs = dict(x=filtered_data_vals, nbinsx=nbins)
                fig = go.Histogram(
                    # x=data[item],
                    **hist_kwargs,
                    histnorm=None,
                )
        else:
            hist_kwargs = dict(x=data[item], nbinsx=nbins)
            fig = go.Histogram(
                # x=data[item],
                **hist_kwargs,
                histnorm=None,
            )

        return fig

    def create_overview_table(self, data=None, title="Experiment design", plot_id="plot_table"):

        widths = [compute_auto_width(self.manifest_data[col]) for col in self.manifest_data.columns]

        # Create a Plotly table.
        header = dict(values=list(self.manifest_data.columns),
                      fill_color='paleturquoise',
                      align='left')
        cells = dict(values=[self.manifest_data[col] for col in self.manifest_data.columns],
                     fill_color='lavender',
                     align='left'
                     )
        table = go.Table(header=header, cells=cells, columnwidth=widths)

        fig = go.Figure(data=[table])

        fig.update_layout(title=title,
                          margin=dict(
                              l=100,  # left margin
                              r=100,  # right margin
                              t=100,  # top margin
                              b=50  # bottom margin
                          ),
                          width=1600,  # final PDF width in pixels
                          height=300 + len(self.manifest_data) * 25,  # final PDF height in pixels
                          template=DEFAULT_TEMPLATE, colorway=NATURE_PALETTE)
        return fig

    def create_gantt_chart(self, title="Workflow Processing Time"):

        if len(self.runtime_dict) == 0:
            return px.scatter()

        total_minutes = sum(self.runtime_dict.values())
        start0 = self.finish_time - dt.timedelta(minutes=total_minutes)

        # 2) build rows
        rows = []
        current = start0
        for task, minutes in self.runtime_dict.items():
            end = current + dt.timedelta(minutes=minutes)
            rows.append({"Task": task, "Start": current, "Finish": end, "Duration": minutes})
            current = end  # next task starts here

        df = pd.DataFrame(rows)

        # 3) build timeline figure
        fig = px.timeline(df, x_start="Start", x_end="Finish", y="Task", title=title)
        fig.update_yaxes(autorange="reversed")
        fig.update_traces(text=df["Duration"].apply(lambda d: f"{d:.2f} min"),
                          textposition="auto",
                          textfont=dict(color="black", size=12)  # ← every label: black, 12 pt)
                          )

        fig.add_annotation(
            text=f"Total Time: {total_minutes:.2f} min",
            xref="paper", yref="paper",
            x=1.0, y=1.05,
            showarrow=False,
            font=dict(size=12, color="red")
        )

        fig.update_layout(
            margin=dict(l=120, r=40, t=90, b=60),
            template=DEFAULT_TEMPLATE,
            colorway=NATURE_PALETTE,
            width=1600,
            height=1000
        )
        return fig

    def create_exp_page3(self, psm_id_bar, pep_id_bar, pro_id_bar, pep_len_dis, miss_cle_dis, charge_diss):

        whole_fig = make_subplots(
            rows=3, cols=3,
            # For row 1, we will use col1 and col3; we leave col2 empty.
            specs=[
                [{}, None, {}],
                [{}, {}, {}],
                [{}, {}, {}]
            ],
            subplot_titles=[
                "Charge", "Peptide Length",  # Row 1: we leave the middle title empty
                "PSM IDs", "Peptide IDs", "Protein IDs",  # Row 2
                "Missed Cleavage", "MS1 Mass Error<br>Before (Grey) vs After Calibration (Black)",
                "MS2 Mass Error<br>Before (Grey) vs After Calibration (Black)", ""
                # Row 3: we leave the last title empty
            ],
            vertical_spacing=0.07,  # Reduced vertical spacing
            horizontal_spacing=0.05  # Reduced horizontal spacing
        )

        # Row 1: Two plots.
        # Plot 1 in (1,1): a simple scatter.
        whole_fig.add_trace(charge_diss, row=1, col=1)
        whole_fig.add_trace(pep_len_dis, row=1, col=3)

        # Row 2: Three plots.
        whole_fig.add_trace(psm_id_bar, row=2, col=1)
        whole_fig.add_trace(pep_id_bar, row=2, col=2)
        whole_fig.add_trace(pro_id_bar, row=2, col=3)

        whole_fig.add_trace(miss_cle_dis, row=3, col=1)

        ms1_mass_error_traces = []
        if "Calib" in self.ms1_mass_df.columns:
            ms1_min, ms1_max = self.ms1_mass_df['Value'].min(), self.ms1_mass_df['Value'].max()
            ms2_min, ms2_max = self.ms2_mass_df['Value'].min(), self.ms2_mass_df['Value'].max()
            all_min = min(ms1_min, ms2_min) * 1.2
            all_max = max(ms1_max, ms2_max) * 1.2
            for exp_name, grp in self.ms1_mass_df.groupby("Calib"):
                ms1_mass_error_traces.append(go.Bar(
                    x=grp["Run"],
                    y=grp["Value"],
                    marker_color=grp["Calib"].map({"Old": "grey", "Calibrated": "black"}),
                ))

            whole_fig.add_trace(ms1_mass_error_traces[1], row=3, col=2)
            whole_fig.add_trace(ms1_mass_error_traces[0], row=3, col=2)
            whole_fig.update_xaxes(title_text="Run", row=3, col=2)
            whole_fig.update_yaxes(title_text="MS1 Mass Error Median (PPM)", row=3, col=2, range=[all_min, all_max])

        ms2_mass_error_traces = []
        if "Calib" in self.ms2_mass_df.columns:
            for exp_name, grp in self.ms2_mass_df.groupby("Calib"):
                ms2_mass_error_traces.append(go.Bar(
                    x=grp["Run"],
                    y=grp["Value"],
                    marker_color=grp["Calib"].map({"Old": "grey", "Calibrated": "black"}),
                ))

            whole_fig.add_trace(ms2_mass_error_traces[1], row=3, col=3)
            whole_fig.add_trace(ms2_mass_error_traces[0], row=3, col=3)
            whole_fig.update_xaxes(title_text="Run", row=3, col=3)
            whole_fig.update_yaxes(title_text="MS2 Mass Error Median (PPM)", row=3, col=3, range=[all_min, all_max])

        whole_fig.update_xaxes(domain=[0, 0.3], row=1, col=1)
        whole_fig.update_xaxes(domain=[0.33, 1], row=1, col=3)

        # --- Update axis labels for each XY subplot ---
        # Row 1:
        whole_fig.update_xaxes(title_text="Charge States", row=1, col=1)
        whole_fig.update_yaxes(title_text="# PSMs", row=1, col=1)
        whole_fig.update_xaxes(title_text="Peptide Length", row=1, col=3)
        whole_fig.update_yaxes(title_text="# PSMs", row=1, col=3)

        # Row 2:
        whole_fig.update_xaxes(title_text="Experiments", row=2, col=1)
        whole_fig.update_yaxes(title_text="# PSMs", row=2, col=1)
        whole_fig.update_xaxes(title_text="Experiments", row=2, col=2)
        whole_fig.update_yaxes(title_text="# Peptides", row=2, col=2)
        whole_fig.update_xaxes(title_text="Experiments", row=2, col=3)
        whole_fig.update_yaxes(title_text="# Proteins", row=2, col=3)

        whole_fig.update_xaxes(title_text="Missed Cleavages", row=3, col=1)
        whole_fig.update_yaxes(title_text="# PSMs", row=3, col=1)

        whole_fig.update_layout(
            title_text="Overall Statistics",
            width=1600,  # final PDF width in pixels
            height=1800,  # final PDF height in pixels
            showlegend=False,
            template=DEFAULT_TEMPLATE,
            colorway=NATURE_PALETTE
        )

        return whole_fig

    def make_composite(self, run_name, images):

        # 2) Render summary table as image
        one_features = self.features_weight[run_name]
        n_rows = len(one_features)
        row_height = 870 / n_rows
        header_height = 30
        table_width = 600

        table_fig = go.Figure(data=[go.Table(
            header=dict(
                values=list(one_features.columns),
                line_color='darkslategray',
                fill_color="grey",
                align=['left', 'center'],
                font=dict(color='white', size=13),
                height=header_height,
            ),
            cells=dict(
                values=[one_features[col] for col in one_features.columns],
                line_color='darkslategray',
                fill_color="white",
                align=['left', 'center'],
                font=dict(color='black', size=12),
                height=row_height,
            )
        )])
        table_fig.update_layout(
            width=table_width,
            height=920,
            margin=dict(l=0, r=10, t=10, b=0)
        )

        # 3) Export table figure to PNG in-memory
        buf_table = BytesIO()
        pio.write_image(table_fig, buf_table, format="png")
        buf_table.seek(0)
        table_img = Image.open(buf_table)

        # 4) Open and resize first PNG to match table height
        first_img = images[0]
        target_h = table_img.height
        scale = target_h / first_img.height  # scale factor for width
        new_size = (int(first_img.width * scale), target_h)
        first_resized = first_img.resize(new_size, Image.LANCZOS)

        # 5) Composite table + first image
        comp_w = table_img.width + first_resized.width
        composite = Image.new("RGB", (comp_w, target_h), "white")
        composite.paste(table_img, (0, 0))
        composite.paste(first_resized, (table_img.width, 0))

        all_images = []
        all_images.append(composite)
        for img in images[1:]:
            # Resize each image to match the table height
            all_images.append(img)

        max_width = max(im.width for im in all_images)
        total_height = sum(im.height for im in all_images)

        # Create a new blank (white) image with the desired composite size.
        all_composite = Image.new("RGB", (max_width, total_height), "white")

        # Paste each image one below the other.
        current_y = 0
        for im in all_images:
            all_composite.paste(im, (0, current_y))
            current_y += im.height

        padded = ImageOps.expand(all_composite, border=20, fill="white")
        png_buffer = BytesIO()
        padded.save(png_buffer, format="PDF")
        png_buffer.seek(0)

        return png_buffer


def main():
    parser = argparse.ArgumentParser(description="FragPipe report generator")
    parser.add_argument("-r", "--results_path", type=str, required=True)
    args = parser.parse_args()
    results_path = args.results_path + "\\"

    pio.kaleido.scope.mathjax = None

    pdf_pages = []
    fragPipeReport = FragPipeReport(results_path=results_path)

    charge_ids = fragPipeReport.distribution_data.groupby('Charge').size().reset_index(name='Count')
    miss_cle_ids = fragPipeReport.distribution_data.groupby('Number of Missed Cleavages').size().reset_index(name='Count')

    running_table_fig = fragPipeReport.create_overview_table()
    running_time_fig = fragPipeReport.create_gantt_chart()

    # Experiment overview
    page1_pdf = fig_to_pdf_bytes(running_table_fig)
    pdf_pages.append(page1_pdf)
    page2_pdf = fig_to_pdf_bytes(running_time_fig)
    pdf_pages.append(page2_pdf)

    psm_id_bar = create_bar_chart(data=fragPipeReport.id_nums,
                                  x="Experiment",
                                  y="PSM")
    pep_id_bar = create_bar_chart(data=fragPipeReport.id_nums,
                                  x="Experiment",
                                  y="Peptides")
    pro_id_bar = create_bar_chart(data=fragPipeReport.id_nums,
                                  x="Experiment",
                                  y="Proteins")
    pep_len_dis = create_distribution_plot(fragPipeReport.distribution_data, item="Peptide Length")
    miss_cle_dis = create_bar_chart(miss_cle_ids, x="Number of Missed Cleavages", y="Count")
    charge_diss = create_bar_chart(charge_ids, x="Charge", y="Count",)

    page3_pdf = fragPipeReport.create_exp_page3(psm_id_bar, pep_id_bar, pro_id_bar, pep_len_dis, miss_cle_dis,
                                                charge_diss)
    pdf_pages.append(fig_to_pdf_bytes(page3_pdf))

    hist_items = ["Peptide Length", "Number of Missed Cleavages", "Charge", "Observed M/Z", "Retention", "Hyperscore",
                  "Delta Mass"]
    scat_pairs = [["Observed M/Z", "Delta Mass"], ["Retention", "Observed M/Z"]]
    for run, data_dict in fragPipeReport.single_run_data.items():
        one_run_figs = []
        # For a distribution chart, we simulate by using the "Score" as numbers and "Comment" as a dummy category.
        print("Run: ", run)
        charge_ids = data_dict.groupby('Charge').size().reset_index(name='Count')
        miss_cle_ids = data_dict.groupby('Number of Missed Cleavages').size().reset_index(
            name='Count')
        for item in hist_items:
            if item == "Number of Missed Cleavages":
                one_fig = create_bar_chart(miss_cle_ids, x="Number of Missed Cleavages", y="Count")
            elif item == "Charge":
                one_fig = create_bar_chart(charge_ids, x="Charge", y="Count")
            else:
                one_fig = fragPipeReport.create_run_distribution_plot(data=data_dict, title=item + "/PepID Chart",
                                                                  item=item)
            one_run_figs.append(one_fig)
        for one_pair in scat_pairs:
            one_fig = fragPipeReport.create_run_scatter_chart(data=data_dict,
                                                              title=one_pair[0] + "/" + one_pair[1] + " Scatter Chart",
                                                              x=one_pair[0], y=one_pair[1])
            one_run_figs.append(one_fig)

        one_run_pdf = create_exp_page_running(one_run_figs, run)
        pdf_pages.append(fig_to_pdf_bytes(one_run_pdf))
    # Convert the MSBooster plots to PDF bytes
    for run_name, images in fragPipeReport.msbooster_plots.items():
        # Convert the images to PDF bytes
        if run_name in fragPipeReport.single_run_data.keys():
            msbooster_pdf = fragPipeReport.make_composite(run_name, images)
            pdf_pages.append(msbooster_pdf)

    final_pdf_buffer = merge_pdf_buffers(pdf_pages)
    # Save the final PDF file.
    print("Saving final PDF report...")
    final_pdf_filename = os.path.join(results_path, "fragpipe-report.pdf")
    with open(final_pdf_filename, "wb") as f:
        f.write(final_pdf_buffer.getvalue())

    print(f"Final PDF report generated as '{final_pdf_filename}'.")


if __name__ == '__main__':
    main()
