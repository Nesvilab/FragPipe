import pathlib, re, sys

"""
In the consensus library, rename runs to a single name to make spectrast2spectrast_irt.py run without error.
"""

p = pathlib.Path(sys.argv[1]).resolve()
pout = pathlib.Path(sys.argv[2]).resolve()

recomp = re.compile("RawSpectrum=(?:.+?) ")


def f(l):
	s, n = recomp.subn("RawSpectrum=consensus_run.1.1 ", l, count=1)
	assert n == 1, l
	return s


with p.open() as fin, pout.open("w") as fout:
	gen = (
		f(l)
		# recomp.sub("RawSpectrum=consensus_run.1.1 ", l, count=1)
		if l.startswith("Comment: ") else
		l
		for l in fin)

	fout.writelines(gen)

print("Unite runs done")
