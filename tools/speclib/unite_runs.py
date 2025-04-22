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
