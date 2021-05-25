import concurrent.futures
import datetime
import io
import itertools
import mmap
import os
import os.path
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
import typing
import warnings

import numpy as np
import pandas as pd

import multiprocessing as mp

if sys.version_info[:2] >= (3, 7):
	sys.stdout.reconfigure(encoding='utf-8')
	sys.stderr.reconfigure(encoding='utf-8')
elif sys.version_info[:2] >= (3, 1):
	sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', line_buffering=True)
	sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', line_buffering=True)
else:
	raise Exception('Python 3.1 or above is required')

def _warning(message, category=UserWarning, filename='', lineno=-1, file=None, line=None):
	print(message)


warnings.showwarning = _warning

argv = sys.argv[1:]
if len(argv) == 0:
	print('Example usage:')
	print('python3 msfragger_pep_split.pyz 3 "java -Xmx10g -jar" msfragger.jar fragger.params *.mzML')
	sys.exit(0)


num_parts_str, jvm_cmd_str, msfragger_jar_path_str, param_path_str, *infiles_str = argv

jvm_cmd = shlex.split(jvm_cmd_str, posix=False)
msfragger_jar_path = pathlib.Path(msfragger_jar_path_str).resolve()
param_path = pathlib.Path(param_path_str)
infiles = [pathlib.Path(e) for e in infiles_str]
jvm_cmd, msfragger_jar_path, param_path, infiles
msfragger_cmd = jvm_cmd + [msfragger_jar_path]
tempdir = pathlib.Path('./split_peptide_index_tempdir')
params_txt = param_path.read_bytes().decode()
output_file_extension_mo = re.compile(r'^output_file_extension *= *(\S+)', re.MULTILINE).search(params_txt)
output_file_extension = output_file_extension_mo.group(1) if output_file_extension_mo else 'pepXML'
output_report_topN = int(re.compile(r'^output_report_topN *= *(\d+)', re.MULTILINE).search(params_txt).group(1))
output_max_expect_mo = re.compile(r'^output_max_expect *= *(\S+)', re.MULTILINE).search(params_txt)
output_max_expect = 50.0 if output_max_expect_mo is None else float(output_max_expect_mo.group(1))
calibrate_mass_mo = re.compile(r'^calibrate_mass *= *([012])\b', re.MULTILINE).search(params_txt)
calibrate_mass: int = int(calibrate_mass_mo.group(1))
assert calibrate_mass in [0, 1, 2]
assert [int(re.compile(r'^calibrate_mass *= *([012])\b', re.MULTILINE).search(a).group(1))
		for a in ['calibrate_mass = 0', 'calibrate_mass = 1', 'calibrate_mass = 2']] == [0, 1, 2]


num_parts = int(num_parts_str)
tempdir_parts = [tempdir / str(i) for i in range(num_parts)]


recomp_fasta = re.compile(r'^database_name\s*=\s*(.+?)(?:\s*#.+)?$', re.MULTILINE)
[fasta_path_str] = recomp_fasta.findall(params_txt)
fasta_path = pathlib.Path(fasta_path_str)
if 0:
	fasta_prots: typing.List[bytes] = [e.rstrip() for e in fasta_path.read_bytes()[1:].split(b'\n>')]

def get_fasta_offsets(fasta_path):

	with fasta_path.open('rb') as fo:
		mm = mmap.mmap(fo.fileno(), 0, access=mmap.ACCESS_READ)
		pos = [0] + [e.start() + 1 for e in re.compile(b'\n>').finditer(mm)]
		startpos = [e[0] for e in np.array_split(pos, num_parts)]
		return [slice(s, e) for s, e in zip(startpos, startpos[1:] + [len(mm)])]


file_parts_offsets = get_fasta_offsets(fasta_path)

fasta_part_paths: typing.List[pathlib.Path] = [tempdir / str(i) / f'{fasta_path.name}' for i in range(num_parts)]
param_part_paths: typing.List[pathlib.Path] = [tempdir / str(i) / param_path.name for i in range(num_parts)]
infiles_name = [e.absolute() for e in infiles]
infiles_symlinks_target_pairs = [(ee / e.name, e) for e in infiles for ee in tempdir_parts]
# cmds = [msfragger_cmd + [param_part_path.name, *infiles_name, '--partial', f'{i}']
# 		for i, param_part_path in zip(range(num_parts), param_part_paths)]

generate_expect_cmd = msfragger_cmd + ['--generate_expect_functions'] + [f.stem + '_scores_histogram.tsv' for f in infiles]

def set_up_directories():
	try:
		shutil.rmtree(tempdir)
	except FileNotFoundError:
		pass
	else:
		print(f'deleted existing temporary directory “{tempdir.resolve(strict=False)}”', flush=True)
	tempdir.mkdir()
	for e in tempdir_parts:
		e.mkdir()
	if 0:
		for fasta_part, fasta_part_path in zip(np.array_split(np.array(fasta_prots, object), num_parts), fasta_part_paths):
			with pathlib.Path(fasta_part_path).open('wb') as f:
				f.writelines(b'>' + e + b'\n' for e in fasta_part)

	# write fasta for each part
	with fasta_path.open('rb') as fo:
		for s, fasta_part_path in zip(file_parts_offsets, fasta_part_paths):
			with pathlib.Path(fasta_part_path).open('wb') as f:
				mm = mmap.mmap(fo.fileno(), 0 if s.stop is None else s.stop, access=mmap.ACCESS_READ)
				mm.seek(s.start)
				shutil.copyfileobj(mm, f)

def write_params(params_txt):
	## write params file for each part
	for param_part_path, fasta_name in zip(param_part_paths, fasta_part_paths):
		param_part_path.write_text(
			recomp_fasta.sub(f'database_name = {fasta_name.name}', params_txt))


def run_msfragger(infiles_name):
	cmds = [msfragger_cmd + [param_part_path.name, *infiles_name, '--partial', f'{i}'] # + (['--split2', '1'] if calibrate_mass else [])
			for i, param_part_path in zip(range(num_parts), param_part_paths)]
	for i, (cmd, cwd) in enumerate(zip(cmds, tempdir_parts), start=1):
		print(f'STARTED: slice {i} of {len(cmds)}', flush=True)
		print(f'{cmd}', flush=True)
		subprocess.run(list(map(os.fspath, cmd)), cwd=cwd, check=True)
		print(f'DONE: slice {i} of {len(cmds)}', flush=True)


##########
def write_combined_scores_histo():
	if False:
		scores_histos = [sum(np.loadtxt(ee / (e.stem + '_scores_histogram.tsv'), dtype=np.uint, delimiter='\t', comments=None) for ee in tempdir_parts)
						 for e in infiles]

	scores_histos = [sum(pd.read_csv(ee / (e.stem + '_scores_histogram.tsv'), dtype=np.uint64, delimiter='\t', header=None, sep='\t').values for ee in tempdir_parts)
					 for e in infiles]
	for f, scores_histo in zip(infiles, scores_histos):
		np.savetxt(tempdir / (f.stem + '_scores_histogram.tsv'), scores_histo, delimiter='\t', fmt='%d')

##########

def get_expect_functions(infile):
	expect_table = np.loadtxt(tempdir / (infile.stem +'_expectscore.tsv'))
	def expect_function(row):
		m_fA0, m_fA1, m_dLimit = row
		def expect(under_f: float):
			dR: float = np.power(10, m_fA0 + m_fA1 * under_f)
			return max(dR, m_dLimit)
		return expect
	return list(map(expect_function, expect_table))


def get_spectrum(p: pathlib.Path):
	rec = re.compile(b'^<spectrum_query .+? index="(.+?)" (?s:.+?)^</spectrum_query>', re.MULTILINE)
	with p.open('rb') as f:
		mm = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
		last_idx = 0
		it = rec.finditer(mm)
		e = next(it, None)
		while True:
			# print(int(e.group(1)),last_idx+1)
			last_idx += 1
			if e is None:
				while True:
					yield b'' # end of file
			elif int(e.group(1)) == last_idx:
				yield e.group()
				e = next(it, None)
			else:
				yield b''

re_search_hit_first_line = re.compile(rb'(?<=hit_rank=")(\d+)(?=" )'
									  b'|'
									  b'(?<=protein=")(.+?)(?=" )')
re_search_hit = re.compile(b'^(<search_hit.+?^</search_hit>)', re.DOTALL | re.MULTILINE)


def step1(spec_queries: typing.Tuple[bytes]) -> (bytes, typing.List[bytes]):
	'get all search hits and search hit headers'
	spec_query_head0 = set(spectrum_query.splitlines()[0] for spectrum_query in spec_queries if len(spectrum_query) > 0)
	if len(spec_query_head0) == 0:
		return None
	if len(spec_query_head0) > 1:  # skip scan when we have multiple charges
		warnings.warn("Input file contains MS/MS scans with no precursor charge state information. All such scans will be skipped when using Split Database option.")
		return None
	spec_query_head, = spec_query_head0
	search_hits = sum([re_search_hit.findall(spectrum_query) for spectrum_query in spec_queries], [])
	search_hit_start_tags = [re_search_hit_first_line.sub(b'{}', search_hit.splitlines()[0]) for search_hit in search_hits]
	header_txts = [(search_hit_tag, search_hit) for search_hit_tag, search_hit in zip(search_hit_start_tags, search_hits)]
	d = {}
	for header, txt in header_txts:
		d.setdefault(header, []).append(txt)
	for k, v in d.items():
		d[k] = v[0]
	return spec_query_head, list(d.values())


re_scores = re.compile(b'''^<search_score name="hyperscore" value="(.+?)"/>
<search_score name="nextscore" value="(.+?)"/>
''', re.MULTILINE)
re_massdiff_scores = re.compile(b'''<search_hit .*? massdiff="(.+?)".*(?s:.*?)
<search_score name="hyperscore" value="(.+?)"/>
<search_score name="nextscore" value="(.+?)"/>
''')
re_massdiff_scores.match(b'''<search_hit peptide="MSKDKANMQHR" massdiff="0.0033" calc_neutral_pep_mass="1376.6288" peptide_next_aa="Y" num_missed_cleavages="10" num_tol_term="2" num_tot_proteins="1" tot_num_ions="40" hit_rank="1" num_matched_ions="7" protein="sp|P31943|HNRH1_HUMAN Heterogeneous nuclear ribonucleoprotein H OS=Homo sapiens OX=9606 GN=HNRNPH1 PE=1 SV=4" peptide_prev_aa="A" is_rejected="0">
<modification_info>
<mod_aminoacid_mass mass="147.0354" position="1"/>
<mod_aminoacid_mass mass="147.0354" position="8"/>
</modification_info>
<search_score name="hyperscore" value="13.29500008"/>
<search_score name="nextscore" value="13.295"/>
<search_score name="expect" value="2.001e-01"/>
</search_hit>
''').groups()
re_update_search_hit = re.compile(rb'''\A(.+hit_rank=")(?:.+?)("(?s:.+?))
<search_score name="hyperscore" value="(?:.+?)"/>
<search_score name="nextscore" value="(?:.+?)"/>
<search_score name="expect" value="(?:.+?)"/>
((?s:.+))\Z''')

def new_spec(expect_func, spectrum_query_parts):
	spectrum_query_header__search_hits = step1(spectrum_query_parts)
	if spectrum_query_header__search_hits is None:
		return b''
	spectrum_query_header, search_hits = spectrum_query_header__search_hits

	def get_scores(search_hit: bytes):
		hyperscore, nextscore = [float(e) for e in re_scores.search(search_hit).groups()]
		return hyperscore, nextscore, expect_func(hyperscore)
	def get_massdiff_and_scores(search_hit: bytes):
		if re_massdiff_scores.search(search_hit) is None:
			print(search_hit)
		massdiff, hyperscore, nextscore = [float(e) for e in re_massdiff_scores.search(search_hit).groups()]
		return massdiff, hyperscore, nextscore, expect_func(hyperscore)

	def step2(search_hits: typing.List[bytes]):
		if 0:
			search_hit_with_scores = (get_scores(search_hit) + (search_hit,) for search_hit in search_hits)
			# sort by hyperscore
			sorted_search_hits0 = sorted(search_hit_with_scores, key=lambda x: x[0], reverse=True)[:output_report_topN]

		search_hit_with_massdiff_and_scores = (get_massdiff_and_scores(search_hit) + (search_hit,) for search_hit in search_hits)
		# sort by hyperscore and massdiff
		sorted_search_hits0 = [e[1:] for e in
			sorted(search_hit_with_massdiff_and_scores, key=lambda x: (1 / x[1], abs(x[0])))[:output_report_topN]]

		sorted_search_hits1 = list(itertools.takewhile(lambda x: x[2] <= output_max_expect, sorted_search_hits0))
		if len(sorted_search_hits1) == 0:
			return [b'']
		new_nextscores = [e[0] for e in sorted_search_hits1][1:] + [min(sorted_search_hits1, key=lambda x: x[1])[1]]
		sorted_search_hits = [(hyperscore, new_nextscore, expectscore, search_hit)
							  for (hyperscore, _, expectscore, search_hit), new_nextscore in
							  zip(sorted_search_hits1, new_nextscores)]

		def make_new_txt(search_hit: bytes, hit_rank, hyperscore, nextscore, expectscore):
			return re_update_search_hit.sub(f'''\\g<1>{hit_rank}\\2
<search_score name="hyperscore" value="{hyperscore:.3f}"/>
<search_score name="nextscore" value="{nextscore:.3f}"/>
<search_score name="expect" value="{expectscore:.3e}"/>
\\3
'''.encode(), search_hit)

		b = [make_new_txt(search_hit, hit_rank, hyperscore, nextscore, expectscore)
						for hit_rank, (hyperscore, nextscore, expectscore, search_hit) in enumerate(sorted_search_hits, 1)]
		return [spectrum_query_header, b'\n<search_result>\n'] + b + [b'</search_result>\n</spectrum_query>\n']

	return step2(search_hits)

# a = itertools.takewhile(lambda x: set(x) != {None}, zip_spec_pos[0])
re_pepxml_header = re.compile(b'''(.+?)^</search_summary>''', re.DOTALL|re.MULTILINE)
def get_pepxml_header(p: pathlib.Path):
	with p.open('rb') as f:
		mmap_length = min(1 << 20, os.path.getsize(p))
		mm= mmap.mmap(f.fileno(), mmap_length, access=mmap.ACCESS_READ)
		try:
			ret = re_pepxml_header.match(mm).group()
		except AttributeError:
			print(p)
			print(mm.read())
			raise
		ret0 = re.compile(b'date="(.+?)"').sub(b'date="%b"', ret)
		return re.compile(b'summary_xml="(.+?)"').sub(b'summary_xml="%b"', ret0)


'''
printf 'java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'"'"'T'"'"'HH:mm:ss").format(java.time.LocalDateTime.now())\n' | /usr/lib/jvm/java-10-jdk/bin/jshell -v
 scala -e 'println(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'"'"'T'"'"'HH:mm:ss").format(java.time.LocalDateTime.now()))'
 python3 -c "import datetime;print(datetime.datetime.now().strftime('%Y-%m-%dT%H:%M:%S'))"
'''

##########

def write_pepxml(infile):
	expect_funcs = get_expect_functions(infile)
	zip_spec_pos=zip(*[get_spectrum(tempdir_part / (infile.stem + '.' + output_file_extension)) for tempdir_part in tempdir_parts])
	pepxml_header, = set([get_pepxml_header(tempdir_part / (infile.stem + '.' + output_file_extension)) for tempdir_part in tempdir_parts])
	outfile = infile.with_suffix('.' + output_file_extension)
	with pathlib.Path(outfile).open('wb') as f:
		f.write(pepxml_header % (datetime.datetime.now().strftime('%Y-%m-%dT%H:%M:%S').encode(), os.fspath(outfile).encode()))
		f.write(b'\n')
		for i, (expect_func, spectrum_query_parts) in enumerate(zip(expect_funcs, zip_spec_pos)):
			f.writelines(new_spec(expect_func, spectrum_query_parts))
			if i % 1024 == 0:
				print(f'Writing: {infile.stem}\tspectrum: {i}')
		f.write(b'</msms_run_summary>\n</msms_pipeline_analysis>\n')

def cpu_count():
	try:
		return len(os.sched_getaffinity(0))
	except AttributeError:
		return os.cpu_count() if os.cpu_count() else 1

def combine_results():
	max_workers0 = min(len(infiles), cpu_count())
	max_workers = min(max_workers0, 61) if sys.platform == 'win32' else max_workers0
	with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as exe:
		fs = [exe.submit(write_pepxml, infile) for infile in infiles]
	for e in fs:
		e.result()

def calibrate(fasta_path_sample, calibrate_mass: int):
	params_path_calibrate = tempdir / param_path.name
	params_txt_new = params_txt
	params_path_calibrate.write_text(recomp_fasta.sub(f'database_name = {fasta_path_sample.relative_to(tempdir)}', params_txt_new))
	calibrate_cmd = msfragger_cmd + [params_path_calibrate.resolve(), '--split1', *infiles_name]
	print(f'{calibrate_cmd}', flush=True)
	p = subprocess.Popen(list(map(os.fspath, calibrate_cmd)), cwd=tempdir, stdout=subprocess.PIPE)
	out = b''
	with p.stdout as f:
		for l in f:
			out += l
			sys.stdout.buffer.write(l)
			sys.stdout.buffer.flush()
	out = out.decode()
	p.wait()
	if p.returncode != 0:
		raise AssertionError(p)
	if False:
		p = subprocess.run(list(map(os.fspath, calibrate_cmd)), cwd=tempdir, check=True, stdout=subprocess.PIPE)
		sys.stdout.buffer.write(p.stdout)
		out = p.stdout.decode()

	# orig_ms1_tol: bool = 'Using the original MS1 tolerance.' in out
	orig_ms1_tol: bool = True
	if not orig_ms1_tol:
		new_ms1_tol, = re.compile('New MS1 tolerance: (.+)').findall(out)
	# new_ms2_tol, = re.compile('New MS2 tolerance: (.+)').findall(out)
	if calibrate_mass == 2:
		new_precursor_true_tol0 = re.compile('New precursor_true_tolerance = (.+) PPM').findall(out)
		new_precursor_true_tol = new_precursor_true_tol0[0] if len(new_precursor_true_tol0) == 1 else None
		new_ms2_tol, = re.compile('New fragment_mass_tolerance = (.+) PPM').findall(out)
		try:
			new_use_topN_peak, =re.compile('New use_topN_peaks = (.+)').findall(out)
		except ValueError:
			new_use_topN_peak = None
		try:
			new_minimum_ratio, = re.compile('New minimum_ratio = (.+)').findall(out)
		except ValueError:
			new_minimum_ratio = None
		try:
			new_intensity_transform, = re.compile('New intensity_transform = (.+)').findall(out)
		except ValueError:
			new_intensity_transform = None
		try:
			new_remove_precursor_peak, = re.compile('New remove_precursor_peaks = (.+)').findall(out)
		except ValueError:
			new_remove_precursor_peak = None

	precursor_mass_units, = re.compile(r'^precursor_mass_units\s*=\s*([01])', re.MULTILINE).findall(params_txt)
	precursor_mass_units = int(precursor_mass_units)
	precursor_mass_lower, = re.compile(r'^precursor_mass_lower\s*=\s*([\S]+)', re.MULTILINE).findall(params_txt)
	precursor_mass_upper, = re.compile(r'^precursor_mass_upper\s*=\s*([\S]+)', re.MULTILINE).findall(params_txt)
	precursor_mass_lower, precursor_mass_upper = float(precursor_mass_lower), float(precursor_mass_upper)
	is_closed_search: bool = (precursor_mass_units == 0 and -1 < precursor_mass_lower < precursor_mass_upper < 1) or \
							 (precursor_mass_units == 1 and -100 < precursor_mass_lower < precursor_mass_upper < 100)

	if not orig_ms1_tol:
		params_txt_new = re.compile(r'^precursor_true_tolerance\s*=\s*([0-9.]+)', re.MULTILINE).sub(
			f'precursor_true_tolerance = {new_ms1_tol}', params_txt_new)
		params_txt_new = re.compile(r'^precursor_true_units\s*=\s*([01])', re.MULTILINE).sub(
			'precursor_true_units = 1', params_txt_new)
		if is_closed_search:
			params_txt_new = re.compile(r'^precursor_mass_units\s*=\s*([01])', re.MULTILINE).sub(
				r'precursor_mass_units = 1', params_txt_new)
			params_txt_new = re.compile(r'^precursor_mass_lower\s*=\s*([\S]+)', re.MULTILINE).sub(
				f'precursor_mass_lower = -{new_ms1_tol}', params_txt_new)
			params_txt_new = re.compile(r'^precursor_mass_upper\s*=\s*([\S]+)', re.MULTILINE).sub(
				f'precursor_mass_upper = {new_ms1_tol}', params_txt_new)
	if calibrate_mass == 2:
		if new_precursor_true_tol is not None:
			params_txt_new = re.compile(r'^precursor_true_tolerance\s*=\s*[0-9.]+', re.MULTILINE).sub(
				f'precursor_true_tolerance = {new_precursor_true_tol}', params_txt_new)
			params_txt_new = re.compile(r'^precursor_true_units\s*=\s*[01]', re.MULTILINE).sub(
				'precursor_true_units = 1', params_txt_new)
		params_txt_new = re.compile(r'^fragment_mass_tolerance\s*=\s*[0-9.]+', re.MULTILINE).sub(
			f'fragment_mass_tolerance = {new_ms2_tol}', params_txt_new)
		params_txt_new = re.compile(r'^fragment_mass_units\s*=\s*[01]', re.MULTILINE).sub(
			'fragment_mass_units = 1', params_txt_new)
		if new_use_topN_peak is not None:
			params_txt_new = re.compile(r'^use_topN_peaks\s*=\s*[0-9]+', re.MULTILINE).sub(
				f'use_topN_peaks = {new_use_topN_peak}', params_txt_new)
		if new_minimum_ratio is not None:
			params_txt_new = re.compile(r'^minimum_ratio\s*=\s*[0-9.]+', re.MULTILINE).sub(
				f'minimum_ratio = {new_minimum_ratio}', params_txt_new)
		if new_intensity_transform is not None:
			params_txt_new = re.compile(r'^intensity_transform\s*=\s*[0-9]', re.MULTILINE).sub(
				f'intensity_transform = {new_intensity_transform}', params_txt_new)
		if new_remove_precursor_peak is not None:
			params_txt_new = re.compile(r'^remove_precursor_peak\s*=\s*[0-9]', re.MULTILINE).sub(
				f'remove_precursor_peak = {new_remove_precursor_peak}', params_txt_new)

	# disable check_spectral_files in split search
	params_txt_new, n = re.compile(r'^check_spectral_files\s*=\s*[0-9]', re.MULTILINE).subn(
		'check_spectral_files = 0', params_txt_new)
	if n == 0:
		params_txt_new += '\ncheck_spectral_files = 0'

	mzBINs0 = [e.with_suffix('.mzBIN_calibrated') for e in infiles_name]
	is_calibrated = [e.with_suffix('.mzBIN_calibrated').exists() for e in infiles_name]
	dests = [(tempdir / mzBin.name).with_suffix('.mzBIN_calibrated')
			if iscali else
			tempdir / orig.name
					 for mzBin, orig, iscali in zip(mzBINs0, infiles_name, is_calibrated)]
	for mzBIN0, orig, dest, iscali in zip(mzBINs0, infiles_name, dests, is_calibrated):
		if iscali:
			shutil.move(mzBIN0, dest)
		else:
			def copy_file_or_dir(src, dst):
				try:
					shutil.copytree(src, dst)
				except NotADirectoryError:
					shutil.copy(src, dst)
			copy_file_or_dir(orig, dest)
	return [e.resolve(strict=True) for e in dests], params_txt_new


def sample_fasta(fasta_path, fasta_path_sample, n):
	fasta_prots: typing.List[bytes] = [b'>' + e + b'\n' for e in fasta_path.read_bytes()[1:].split(b'\n>')]
	sample_prots = sorted(fasta_prots)[::n]
	with fasta_path_sample.open('wb') as f:
		f.writelines(sample_prots)
	with fasta_path_sample.with_suffix('.sorted_fasta').open('wb') as f:
		f.writelines(sorted(fasta_prots))

'''
fasta_path = pathlib.Path('/home/ci/msfragger_split_peptides/Uniprot.20160729.Hs.revDecoyPeps.fa').resolve()
fasta_path_sample = pathlib.Path('/home/ci/msfragger_split_peptides/Uniprot.20160729.Hs.revDecoyPeps_sorted.fa')
sample_fasta(fasta_path, fasta_path_sample, 3)
'''

def main():
	# mp.set_start_method('spawn')
	set_up_directories()
	if calibrate_mass in [1, 2]:
		fasta_path_sample = tempdir / fasta_path.name
		sample_fasta(fasta_path, fasta_path_sample, min(num_parts, 1))
		calibrate_mzBIN, params_txt_new = calibrate(fasta_path_sample, calibrate_mass)
	write_params(params_txt_new if calibrate_mass in [1, 2] else params_txt)
	run_msfragger(calibrate_mzBIN if calibrate_mass in [1, 2] else infiles_name)

	write_combined_scores_histo()
	print(f'{generate_expect_cmd}')
	subprocess.run(list(map(os.fspath, generate_expect_cmd)), cwd=tempdir, check=True)

	combine_results()

	shutil.rmtree(tempdir)

if __name__ == '__main__':
	main()
