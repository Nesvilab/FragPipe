#!/usr/bin/env python3

use_philosopher_fo: bool = not True

from common_funcs import (raise_if, raise_if_not, str_to_path, unexpanduser_quote, list_as_shell_cmd, name_no_ext, strIII, os_fspath)
from detect_decoy_prefix import detect_decoy_prefix

import enum
import itertools
import lxml.etree
import os
import pathlib
import re
import shlex
import subprocess
import sys
import shutil
import pandas as pd

def to_windows(cmd):
	r"""convert linux sh scripts to windows

	>>> cmd = r'''
	... set -o xtrace -o errexit -o nounset -o pipefail
	... # some comments
	... echo 1
	... 	echo 2
	... echo 3
	... echo abc\abc'''
	>>> to_windows(cmd)
	['echo 1', 'echo 2', 'echo 3', 'echo abc\\abc']
	"""
	gen0 = (shlex.split(lcmd, comments=True, posix=False) for lcmd in cmd.strip().splitlines())
	return [' '.join(lcmd) for lcmd in gen0 if lcmd and lcmd[0] != 'set']


def adjust_command(cmd):
	'transform multiline Linux commands to work on Windows'
	assert sys.platform in ('linux', 'win32')
	if sys.platform == 'linux':
		return cmd
	return ' && '.join(to_windows(cmd))


# if __name__=='__main__':
# def main():
import doctest

doctest.testmod(verbose=False)

script_dir = pathlib.Path(__file__).resolve().parent
if script_dir.suffix == '.pyz':
	script_dir = script_dir.parent

SPECTRAST_PATH = (script_dir / {'linux': 'linux/spectrast', 'win32': 'win/spectrast.exe'}[sys.platform]).resolve(strict=True)
fasta = str_to_path(sys.argv[1]).resolve(strict=True)
# msproteomicstools_bin_path = str_to_path(sys.argv[3]).resolve(strict=True)
# msproteomicstools_bin_path = str_to_path(pathlib.Path(sys.executable).parent).resolve(strict=True)
iproph_RT_aligned = str_to_path(sys.argv[2]).resolve(strict=True)
prot_xml_file = str_to_path(sys.argv[3]).resolve(strict=True)
output_directory = str_to_path(sys.argv[4])
overwrite = False
if len(sys.argv) >= 6:
	if sys.argv[5].casefold() == 'true':
		overwrite = True

if 'PATHEXT' in os.environ:
	os.environ['PATHEXT'] = '.py' + os.pathsep + os.environ['PATHEXT']
os.environ['PATH'] = os.getcwd() + os.pathsep + os.environ['PATH']

which_philosopher = shutil.which('philosopher')
if which_philosopher is None:
	try:
		philosopher = str_to_path(sys.argv[6]).resolve(strict=True)
	except (FileNotFoundError, IndexError):
		print("Philosopher not found on path and not provided as 6th argument. Exiting.", file=sys.stderr)
		sys.exit(1)
else:
	philosopher = pathlib.Path(which_philosopher)
philosopher = philosopher.resolve(strict=True)
# msproteomicstools_path = pathlib.Path('/storage/teog/anaconda3/bin')
align_with_iRT: bool = True

# spectrast2spectrast_irt_py_path = msproteomicstools_bin_path / 'spectrast2spectrast_irt.py'
# spectrast2tsv_py_path = msproteomicstools_bin_path / 'spectrast2tsv.py'
spectrast2spectrast_irt_py_path = pathlib.Path(shutil.which('spectrast2spectrast_irt.py'))
spectrast2tsv_py_path = pathlib.Path(shutil.which('spectrast2tsv.py'))

assert philosopher.exists()
assert spectrast2spectrast_irt_py_path.exists()
assert spectrast2tsv_py_path.exists()

print("\n".join(map(str, [SPECTRAST_PATH, fasta, iproph_RT_aligned])))

CWD = os.getcwd()

# fasta = pathlib.Path("/data/nesvi/fasta/uniprot-human_20150619_iRT_rev.fasta")
# decoy_prefix = "rev_"
with fasta.open("rb") as f:
	decoy_prefix = detect_decoy_prefix(f)

splib_original = output_directory / "input.splib"
splib_original_backup = output_directory / "input.splib_orig.txt"
splib_new = output_directory / "input_Qcombined.splib"

philosopher_filter_log_path = output_directory / 'filter.log'
peptide_tsv_path = output_directory / 'peptide.tsv'
use_peptide_tsv: bool = peptide_tsv_path.exists()
assert peptide_tsv_path.exists()==philosopher_filter_log_path.exists()






def get_window_setup(p: pathlib.Path):
	"""return a list of pairs of window centers and wideness"""
	l = []
	window_desc = []
	first_precursorScanNum = None
	with p.open("rb") as f:
		# https://bugs.python.org/issue18304
		context = lxml.etree.iterparse(f, events=("start",), tag="{*}precursorMz")
		for action, elem in context:
			precursorScanNum = int(elem.attrib["precursorScanNum"])
			if first_precursorScanNum is None:
				first_precursorScanNum = precursorScanNum
			if first_precursorScanNum != precursorScanNum:
				break

			windowWideness = float(elem.attrib['windowWideness'])
			windowcenter = float(elem.text)

			# https://stackoverflow.com/questions/7171140/using-python-iterparse-for-large-xml-files/7171543
			elem.clear()
			for ancestor in elem.xpath('ancestor-or-self::*'):
				while ancestor.getprevious() is not None:
					del ancestor.getparent()[0]

			window_desc.append((windowcenter, windowWideness))
		del context
	return window_desc


# single_mzXML_path = pathlib.Path("/home/ci/tmp/New Folder/20160801_1ug_HeLa_DIA_10Da_180min_Alexey1.mzXML")
# tail -n+30400 '/home/ci/tmp/New Folder/20160801_1ug_HeLa_DIA_10Da_180min_Alexey1.mzXML' | head -100 > 111.mzXML
# swathwindowssetup = [(center - wideness / 2, center + wideness / 2)
# 					 for center, wideness in get_window_setup(single_mzXML_path)]

# txt = "\n".join("{}\t{}".format(beg, end) for beg, end in swathwindowssetup) + "\n"
# print(txt)


iproph_pep_xmls = sorted(e.resolve() for e in iproph_RT_aligned.glob("*.pep.xml"))
assert len(iproph_pep_xmls) > 0, iproph_RT_aligned

def pred_DIA_Umpire_output():
	endswith_Q123 = set(re.compile(r"_Q[123].(?:iproph)?.pep.xml\Z").search(e.name) is not None
						for e in iproph_pep_xmls)
	assert len(endswith_Q123) == 1, endswith_Q123
	is_DIA_Umpire_output, = endswith_Q123
	return is_DIA_Umpire_output

is_DIA_Umpire_output = pred_DIA_Umpire_output()

def modify_splib():
	recomp = re.compile(r"(Comment:.+\bRawSpectrum=)(.+_Q[123])(\.[0-9]+\.[0-9]+ .+\n)")
	with splib_original.open() as f, \
			splib_new.open("wt") as fout:
		for line in f:
			if line.startswith("Comment:"):
				(beg, raw_spectrum, end) = recomp.fullmatch(line).groups()
				line2 = beg + raw_spectrum[:-2] + "Qstar" + end
			else:
				line2 = line
			fout.write(line2)

	assert not splib_original_backup.exists()
	splib_original.rename(splib_original_backup)
	splib_new.rename(splib_original)


### RT alignment with common peptides
# https://github.com/msproteomicstools/msproteomicstools/blob/17e9c2bf43/analysis/spectral_libs/spectrast2spectrast_irt.py#L331
biognosys_rtkit = {'LGGNEQVTR': -28.3083, 'GAGSSEPVTGLDAK': 0.227424, 'VEATFGVDESNAK': 13.1078, 'YILAGVENSK': 22.3798, 'TPVISGGPYEYR': 28.9999, 'TPVITGAPYEYR': 33.6311, 'DGLDAASYYAPVR': 43.2819, 'ADVTPADFSEWSK': 54.969, 'GTFIIDPGGVIR': 71.3819, 'GTFIIDPAAVIR': 86.7152, 'LFLQFGAQGSPFLK': 98.0897}
import operator

biognosys_rtkit_sorted = sorted(biognosys_rtkit.items(), key=operator.itemgetter(1))
"""
Identification of a Set of Conserved Eukaryotic Internal Retention Time Standards for Data-independent Acquisition Mass Spectrometry.
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4597153/
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4597153/bin/supp_O114.042267_mcp.O114.042267-2.txt
"""
ciRT_rtkit_str = "ADTLDPALLRPGR:35.987015,AFEEAEK:-21.35736,AFLIEEQK:22.8,AGFAGDDAPR:-9.819255,AGLQFPVGR:37.04898,AILGSVER:5.415375,APGFGDNR:-15.63474,AQIWDTAGQER:16.854875,ATAGDTHLGGEDFDNR:3.185526667,ATIGADFLTK:43.837285,AVANQTSATFLR:19.24765,AVFPSIVGRPR:34.03497,C[160]ATITPDEAR:-10.13943,DAGTIAGLNVLR:59.03744667,DAHQSLLATR:-3.25497,DELTLEGIK:39.132035,DLMAC[160]AQTGSGK:0.306955,DLTDYLMK:60.01111111,DNIQGITKPAIR:12.598125,DNTGYDLK:-9.39721,DSTLIMQLLR:103.65,DSYVGDEAQSK:-15.509125,DVQEIFR:29.61571,DWNVDLIPK:70.53546,EAYPGDVFYLHSR:46.35,EC[160]ADLWPR:28.711905,EDAANNYAR:-23.23042,EGIPPDQQR:-15.8411,EHAALEPR:-22.61094,EIAQDFK:-4.04913,EIQTAVR:-17.07064,ELIIGDR:11.56179,ELISNASDALDK:23.50069,EMVELPLR:47.96546,ESTLHLVLR:28.54494,EVDIGIPDATGR:37.10299,FDDGAGGDNEVQR:-11.31703,FDLMYAK:38.2,FDNLYGC[160]R:9.6064,FEELC[160]ADLFR:73.5,FELSGIPPAPR:52.5,FELTGIPPAPR:53.1,FPFAANSR:18.76225,FQSLGVAFYR:60.2276,FTQAGSEVSALLGR:61.450335,FTVDLPK:37.86026,FVIGGPQGDAGLTGR:40.551975,GC[160]EVVVSGK:-15.49014,GEEILSGAQR:-1.811165,GILFVGSGVSGGEEGAR:51.15,GILLYGPPGTGK:45.36582,GIRPAINVGLSVSR:37.98295,GNHEC[160]ASINR:-23.57003,GVC[160]TEAGMYALR:31.20584,GVLLYGPPGTGK:28.11667,GVLMYGPPGTGK:28.20674,HFSVEGQLEFR:41.108635,HITIFSPEGR:22.39813,HLQLAIR:9.42694,HLTGEFEK:-13.72484,HVFGQAAK:-24.54245,IC[160]DFGLAR:28.009545,IC[160]GDIHGQYYDLLR:50.34788,IETLDPALIRPGR:43.43414,IGGIGTVPVGR:21.9,IGLFGGAGVGK:43.285185,IGPLGLSPK:29.48313,IHETNLK:-25.53888,IINEPTAAAIAYGLDK:65.72006667,IYGFYDEC[160]K:31.695135,KPLLESGTLGTK:9.057185,LAEQAER:-25.089125,LGANSLLDLVVFGR:134.00759,LIEDFLAR:56.93148,LILIESR:28.145215,LPLQDVYK:29.2,LQIWDTAGQER:36.28872,LVIVGDGAC[160]GK:10.8,LVLVGDGGTGK:12.022895,LYQVEYAFK:46.26582,MLSC[160]AGADR:-15.49156,NILGGTVFR:49.61455,NIVEAAAVR:5.73971,NLLSVAYK:34.34229,NLQYYDISAK:25.8,NMSVIAHVDHGK:-5.36295,QAVDVSPLR:11.34271,QTVAVGVIK:9.9,SAPSTGGVK:-27.56682,SGQGAFGNMC[160]R:0.790505,SNYNFEKPFLWLAR:96.01717,STELLIR:18.1,STTTGHLIYK:-9.48751,SYELPDGQVITIGNER:67.30002,TIAMDGTEGLVR:32.822885,TIVMGASFR:29.53023,TLSDYNIQK:4.35,TTIFSPEGR:15.183785,TTPSYVAFTDTER:33.79824667,TTVEYLIK:30.16799,VAVVAGYGDVGK:15.331395,VC[160]ENIPIVLC[160]GNK:49.065875,VLPSIVNEVLK:83.750085,VPAINVNDSVTK:17.70942,VSTEVDAR:-20.136945,VVPGYGHAVLR:8.61752,WPFWLSPR:98.382385,YAWVLDK:41.6,YDSTHGR:-57.05955,YFPTQALNFAFK:95.4,YLVLDEADR:27.6947,YPIEHGIVTNWDDMEK:56.9,YTQSNSVC[160]YAK:-12.794935"
ciRT_rtkit = {e.split(":")[0]: float(e.split(":")[1]) for e in ciRT_rtkit_str.split(",")}
ciRT_rtkit_sorted = sorted(ciRT_rtkit.items(), key=operator.itemgetter(1))
len(biognosys_rtkit), len(ciRT_rtkit)
combined_rtkit_str = ",".join(a+":"+str(b) for a,b in sorted({**biognosys_rtkit,**ciRT_rtkit}.items(), key=operator.itemgetter(1)))
rtkit_str = ",".join(a + ":" + str(b) for a, b in sorted({**biognosys_rtkit}.items(), key=operator.itemgetter(1)))

TEMP_FILES = ['input000.splib', 'input000.spidx', 'input000.pepidx',
			  'input.splib',
			  'input_irt.pepidx', 'input_irt.splib', 'input_irt.csv',
			  'output_file_irt_con000.splib', 'output_file_irt_con000.spidx', 'output_file_irt_con000.pepidx',
			  'output_file_irt_con001.splib', 'output_file_irt_con.splib', 'output_irt_con.tsv',
			  'con_lib_not_in_psm_tsv.tsv']

spectrast_cmds_part1= fr'''
set -o xtrace -o errexit
{sys.executable} {script_dir / "spectrast_gen_pepidx.py"} -i input.splib -o input_irt.splib # generate .pepidx file
#outfiles: input_irt.splib, input_irt.csv
## Consolidate the library into a single consensus spectrum entry for each peptide sequence.
{SPECTRAST_PATH} -M spectrast.usermods -cAC -c_BIN! -cIHCD -cNoutput_file_irt_con000 input_irt.splib
#outfile:output_file_irt_con000.splib
{sys.executable} {script_dir / "unite_runs.py"} output_file_irt_con000.splib output_file_irt_con001.splib
#outfile:output_file_irt_con001.splib
'''

spectrast_cmds_part2= fr"""
### if self aligned, use iRT alignment here.
{sys.executable} {spectrast2spectrast_irt_py_path} --kit {rtkit_str} --rsq_threshold=0.25 -r -i output_file_irt_con001.splib -o output_file_irt_con.splib
#outfile:output_file_irt_con.splib
"""

spectrast_cmds_part3=fr"""
## Filter the consensus splib library into a transition list
{sys.executable} {spectrast2tsv_py_path} -l 300,2000 -s b,y -x 1,2 -o 3 -n 6 -p 0.05 -d -e -k openswath -a output_irt_con.tsv output_file_irt_con.splib
#outfile:output_irt_con.tsv
"""


spectrast_cmds = spectrast_cmds_part1 + spectrast_cmds_part2 + spectrast_cmds_part3

phi_log = output_directory / "philosopher.log"

phi_cmd_part1= f"""
set -xe
{philosopher} workspace --clean --analytics false
{philosopher} workspace --init --analytics false
{philosopher} database --annotate {fasta}
{philosopher} filter \
	--razor \
	--sequential \
	--pepxml {iproph_RT_aligned}/ \
	--protxml {prot_xml_file} \
	--tag {decoy_prefix}\
	{'--fo' if use_philosopher_fo else ''}"""

r"""
do not use:
	--pepxml {iproph_RT_aligned}/*.iproph.pep.xml
will only take the first pepxml, no warnings
"""

phi_cmd_part2 = f"{philosopher} report"


class Filter_option(enum.Enum):
	all = 0
	by_2D_filtering = 1

def get_pep_ion_minprob_from_log(opt: Filter_option, philosopher_filter_log: str):
	res2 = [float(e) for e in re.compile(' Ions.+threshold.*?=([0-9.]+)').findall(philosopher_filter_log)]
	return res2[opt.value]

def get_pep_ion_minprob(opt: Filter_option, philosopher_filter_log: str = None):
	if philosopher_filter_log is not None:
		return get_pep_ion_minprob_from_log(opt, philosopher_filter_log)
	outl=[]
	f=sys.stdout.buffer
	### get 2D FDR
	if sys.platform=='linux':
		with subprocess.Popen(adjust_command(phi_cmd_part1), shell=True, stderr=subprocess.PIPE, cwd=os_fspath(output_directory)) as proc1, \
			phi_log.open('wb') as f2:
			for line in proc1.stderr:
				f.write(line)
				f.flush()
				f2.write(line)
				f2.flush()
				outl.append(line)
	if sys.platform=='win32':
		with subprocess.Popen(adjust_command(phi_cmd_part1), shell=True, stdout=subprocess.PIPE, cwd=os_fspath(output_directory)) as proc1, \
			phi_log.open('wb') as f2:
			for line in proc1.stdout:
				f.write(line)
				f.flush()
				f2.write(line)
				f2.flush()
				outl.append(line)
	if use_philosopher_fo and proc1.returncode == 1:
		a = (output_directory/'.meta'/'pep_pro_mappings.tsv').read_text()
		l = [e.split('\t') for e in re.compile('(?=sp\\|)').split(a)]
		d = {ee2: e for e, *e2 in l for ee2 in e2}
		##create dummy fasta
		with (output_directory / 'proteins.fas').open('x') as f:
			for prot in sorted(set(d.values())):
				f.write(f'>{prot}\nDUMMY\n')
		# create dummy psm.tsv
		(output_directory / 'psm.tsv').write_text(
			'Peptide\tProtein\n' +
			'\n'.join(f'{ee2}\t{e}' for e, *e2 in l for ee2 in e2)
		)
	else:
		assert proc1.returncode == 0, [proc1.args, proc1.returncode]
		## filter original fasta file
		subprocess.run(phi_cmd_part2, shell=True, stderr=subprocess.STDOUT, cwd=os_fspath(output_directory), check=True)
	out = b"".join(filter(lambda line: not line.startswith(b"+"), outl))
	outtxt = out.decode("ascii")
	res2 = [float(e) for e in re.compile(' Ions.+threshold.*?=([0-9.]+)').findall(outtxt)]
	return res2[opt.value]

	log_kvs = [list(g) for _, g in itertools.groupby(shlex.split(outtxt), key=lambda x: x.startswith("time="))]
	logrecords = [dict(e.split("=", 1) for e in a + b)
				  for a, b in zip(log_kvs[::2], log_kvs[1::2])]
	res2 = [float(e["threshold"]) for e in logrecords if e["msg"].endswith("Ions")]
	assert len(res2) == 2, res2
	return res2[opt.value]

def spectrast_cmd(prob):
	return [
			   os_fspath(SPECTRAST_PATH),
			   '-M', 'spectrast.usermods',
			   "-c_BIN!",
			   f"-cP{prob}",
			   "-cIHCD", f"-cN{output_directory / 'input000'}"] + \
		   list(map(os_fspath, iproph_pep_xmls))


allcmds = "\n".join([phi_cmd_part1, phi_cmd_part2, " ".join(spectrast_cmd("{}")), spectrast_cmds])



def filter_proteins(fasta, decoy_prefix):
	"""filter out proteins that are not in the protein list by philosopher (proteins.fas)
	"""

	### get the iRT protein sequences
	gen = (e.split("\n", 1)[0].split(' ', 1)[0] for e in fasta.read_text()[1:].split("\n>"))
	irt_prots_with_decoys = [e for e in gen if "iRT" in e]  # decoy sequences included
	# assert len(irt_prots_with_decoys) % 2 == 0, irt_prots_with_decoys
	irt_prots = frozenset(e for e in irt_prots_with_decoys if not e.startswith(decoy_prefix))
	# assert len(irt_prots_with_decoys) == len(irt_prots) * 2, irt_prots
	# print("irt_prots_with_decoys",irt_prots_with_decoys)
	# print("irt_prots",irt_prots)
	if use_peptide_tsv:
		philosopher_peptide_tsv = pd.read_table(peptide_tsv_path)
		proteins_fas = frozenset(philosopher_peptide_tsv['Protein'])
	else:
		fasta_file = output_directory / "proteins.fas"
		# gen0 = (e.split("\n", 1)[0].split(' ', 1)[0] for e in fasta_file.read_text()[1:].split("\n>"))
		# proteins_fas = frozenset(filter(lambda x: not x.startswith(decoy_prefix), gen0))
		proteins_fas = frozenset(e.split("\n", 1)[0].split(' ', 1)[0] for e in fasta_file.read_text()[1:].split("\n>"))

	recomp = re.compile(r"(Comment: .+\bProtein=)(?:\d+)/(\S+)(.+\n)")

	from typing import List, Optional
	def handle_unit_OLD(lines: List[str]) -> Optional[List[str]]:
		mos = map(recomp.fullmatch, lines)
		[(comment_idx, mo)] = [(i, mo) for i, mo in enumerate(mos) if mo is not None]
		prot_list_str = mo.group(2)
		prot_list = frozenset(prot_list_str.split("/"))
		prot_list_filt = (prot_list & proteins_fas) | \
						 frozenset(e for e in prot_list_str.split("/") if e in irt_prots)
						# frozenset(e for e in prot_list_str.split("/") if "iRT" in e)
		idx = lines.index(f"LibID: {libID_orig}\n")
		lines[idx] = f"LibID: {libID}\n"
		if prot_list != prot_list_filt:
			prot_list_str_new = "/".join([str(len(prot_list_filt))] + sorted(prot_list_filt))
			lines[comment_idx] = mo.group(1) + shlex.quote(prot_list_str_new) + mo.group(3)

		# if len(prot_list_filt) > 0:
		# fout.writelines(txt)
		# fout.write("\n")
		# libID += 1
		return None \
			if len(prot_list_filt) == 0 else \
			lines

	recomp3 = re.compile(r'(Comment: .+\bProtein=)'
						 r'(?:(?:\d+)/(\S+)|"(?:\d+)/(.+?)")'  # with double quotes
						 r'(.+\n)')
	def handle_unit(lines: List[str]) -> Optional[List[str]]:
		mos = map(recomp3.fullmatch, lines)
		[(comment_idx, mo)] = [(i, mo) for i, mo in enumerate(mos) if mo is not None]
		groups = [e for e in mo.groups() if e is not None]
		prot_list_str = groups[1]
		prot_list = frozenset(prot_list_str.split("/"))
		prot_list_filt = (prot_list & proteins_fas) | \
						 frozenset(e for e in prot_list_str.split("/") if e in irt_prots)
		idx = lines.index(f"LibID: {libID_orig}\n")
		lines[idx] = f"LibID: {libID}\n"
		if prot_list != prot_list_filt:
			prot_list_str_new = "/".join([str(len(prot_list_filt))] + sorted(prot_list_filt))
			lines[comment_idx] = groups[0] + shlex.quote(prot_list_str_new) + groups[2]

		return None \
			if len(prot_list_filt) == 0 else \
			lines

	with (output_directory/"input000.splib").open("rt") as f, \
			(output_directory / "input.splib").open("wt") as fout:
		libID, libID_orig = 0, 0
		for line in f:
			if not line.startswith("Name: "):
				fout.write(line)
				continue
			txt = [line]
			for line in iter(f.readline, "\n"):
				txt.append(line)
			outtxt=handle_unit(txt)
			if outtxt is not None:
				fout.writelines(outtxt)
				fout.write("\n")
				libID+=1
			libID_orig += 1


def main0():

	output_directory.mkdir(exist_ok=overwrite)
	print("running:\n" + allcmds, flush=True)
	(output_directory / "cmds.txt").write_text(allcmds)
	pep_ion_minprob=get_pep_ion_minprob(
		Filter_option.all
		# Filter_option.by_2D_filtering
		,
		philosopher_filter_log_path.read_text() if philosopher_filter_log_path.exists() else None
	)
	# http://tools.proteomecenter.org/wiki/index.php?title=Software:SpectraST#User-defined_Modifications
	# http://tools.proteomecenter.org/wiki/index.php?title=Spectrast.usermods
	(output_directory / 'spectrast.usermods').write_text(
		r'''M|+16|
C|+57|
n|+42|
C|119.004099|Cysteinyl
c|-0.984016|Amidated''')
	cmd2 = " ".join(spectrast_cmd(pep_ion_minprob))
	print(f"running:\n{cmd2}\n…")
	(output_directory / "cmds2.txt").write_text(cmd2)
	subprocess.run(spectrast_cmd(pep_ion_minprob), cwd=os_fspath(output_directory), check=True)

	print("take only proteins from philosopher’s proteins.fas…")
	filter_proteins(fasta, decoy_prefix)
	# swathwindowssetup_file_path.write_text(txt, "ascii")
	if is_DIA_Umpire_output:
		print("modifying splib file to combine Q[123] from DIA-umpire…")
		modify_splib()
	# %%time
	# subprocess.run(spectrast_cmds, shell=True, cwd=os_fspath(output_directory), check=True)
	subprocess.run(adjust_command(spectrast_cmds_part1), shell=True, cwd=os_fspath(output_directory), check=True)
	if align_with_iRT:
		cp = subprocess.run(adjust_command(spectrast_cmds_part2), shell=True, cwd=os_fspath(output_directory), check=not True,
							stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
		if cp.returncode != 0:
			print('Skipping iRT alignment')
			shutil.move(output_directory / 'output_file_irt_con001.splib', output_directory / 'output_file_irt_con.splib')
		else:
			print(cp.stdout)
	else:
		shutil.move(output_directory / 'output_file_irt_con001.splib', output_directory / 'output_file_irt_con.splib')
	subprocess.run(adjust_command(spectrast_cmds_part3), shell=True, cwd=os_fspath(output_directory), check=True)




##### multiple protein assignment to peptide reduced to single protein
# https://github.com/Nesvilab/Protid_largedatasets/blob/master/protxmlDtFdr.R
from typing import List


def get_prot_group_infos2(p: pathlib.Path):
	import mmap, re
	import lxml.etree
	def number_id_peps__pep_prob_sum(prot):
		peps = prot.findall("peptide")
		return (len(peps), sum(float(pep.get("nsp_adjusted_probability")) for pep in peps))
	prot_event = re.compile(re.escape(b"<protein ") + b".+?" + re.escape(b"</protein>"), re.DOTALL)
	with p.open("rb") as f:
		mm = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
		gen = (lxml.etree.fromstring(e.group()) for e in prot_event.finditer(mm))
		return [(prot.get('protein_name'), [e.get('protein_name') for e in prot.findall("indistinguishable_protein")], number_id_peps__pep_prob_sum(prot))
				for prot in gen]

def get_pep_init_prob(p: pathlib.Path):
	import mmap, re
	import lxml.etree
	prot_event = re.compile(re.escape(b"<peptide ") + b".+?" + re.escape(b"</peptide>"), re.DOTALL)
	d = {}
	with p.open("rb") as f:
		mm = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
		gen = (lxml.etree.fromstring(e.group()) for e in prot_event.finditer(mm))
		for pep in gen:
			d.setdefault(pep.get("peptide_sequence"), []).append(pep.get("initial_probability"))
	return d

def get_prot_group_infos(p: pathlib.Path):
	import lxml.etree
	root = lxml.etree.parse(os_fspath(p)).getroot()
	def number_id_peps__pep_prob_sum(prot):
		peps = prot.findall("{*}peptide")
		return (len(peps), sum(float(pep.get("nsp_adjusted_probability")) for pep in peps))

	return [(prot.get('protein_name'), [e.get('protein_name') for e in prot.findall("{*}indistinguishable_protein")], number_id_peps__pep_prob_sum(prot))
			for prot in root.iterfind(".//{*}protein_group/{*}protein")]



def get_prot_razor_dict(l) -> dict:
	"""get dict of proteins to (# of ided peptides, sum of pep probs) for razor peptide selection from prot.xml"""
	return {representative_protein: number_id_peps__pep_prob_sum_pair
			for representative_protein, _, number_id_peps__pep_prob_sum_pair in l}


def edit_raw_con_lib():
	# p = data_directory / "ProteinProphet/interact.prot.xml"
	p = prot_xml_file
	import concurrent.futures
	 # with concurrent.futures.ProcessPoolExecutor(1) as exe:
		# l = exe.submit(get_prot_group_infos2, p).result()
	l = get_prot_group_infos2(p)
	# l = get_prot_group_infos(p)
	to_rep_prot_dict = {prot: representative_protein
						for representative_protein, indistinguishable_proteins, _ in l
						for prot in indistinguishable_proteins}
	# for prot in [representative_protein] + indistinguishable_proteins}

	def proteinName_to_list(s: str) -> List[str]:
		numstr, *protlist = s.split("/")
		assert int(numstr) == len(protlist)
		# return sorted({to_rep_prot_dict[e] for e in protlist})
		return sorted({to_rep_prot_dict.get(e, e) for e in protlist})

	prot_razor_dict = get_prot_razor_dict(l)

	def select_one_prot(protlist: List[str]) -> str:
		# print(sorted([(prot_razor_dict.get(prot, (0, 0.0)), prot) for prot in protlist]))
		return max(protlist, key=lambda x: prot_razor_dict.get(x, (0, 0.0)))
		return max(protlist, key=prot_razor_dict.__getitem__)
		m = max([(prot_razor_dict[prot], prot) for prot in protlist])
		return m[1]

	import pandas as pd, pathlib
	t = pd.read_table("output_irt_con.tsv")
	if use_peptide_tsv:
		philosopher_peptide_tsv = pd.read_table(peptide_tsv_path)
		pep_to_razor_prot = dict(philosopher_peptide_tsv[["Peptide", "Protein"]].itertuples(index=False))
	else:
		philosopher_psm_tsv = pd.read_table('psm.tsv')
		pep_to_razor_prot = {pep: razor_prot for pep, razor_prot in philosopher_psm_tsv[["Peptide", "Protein"]].itertuples(index=False)}
	t["razor_Protein"] = t["PeptideSequence"].map(pep_to_razor_prot.get)
	pep_init_prob = get_pep_init_prob(p)
	pathlib.Path('con_lib_not_in_psm_tsv.tsv').write_text(
		t[t["razor_Protein"].isnull()].assign(init_prob=t["PeptideSequence"].map(pep_init_prob.get).map(lambda x: "" if x is None else ','.join(x)))
			.to_csv(sep='\t', index=False).replace('(UniMod:5)', '(UniMod:1)')
	)
	fout = pathlib.Path('con_lib.tsv')
	print(f'writing {fout.resolve()}')
	fout.write_text(
		t[t["razor_Protein"].notnull()].to_csv(sep='\t', index=False).replace('(UniMod:5)', '(UniMod:1)')
	)


main0()

os.chdir(os_fspath(output_directory))
edit_raw_con_lib()
for f in TEMP_FILES:
	try:
		pathlib.Path(f).unlink()
	except FileNotFoundError as e:
		pass
os.chdir(CWD)

# if __name__=='__main__':
# 	main()