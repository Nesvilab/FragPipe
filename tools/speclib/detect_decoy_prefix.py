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

#!/usr/bin/env python3
import collections
import sys
from typing import List, Tuple, IO


def detect_decoy_prefix_old(bio: IO[bytes]) -> str:
	"""detect decoy prefix from fasta file"""
	# descs = [e.splitlines()[0] for e in bio.read()[1:].split(b'>')]
	descs = [l[1:].rstrip(b"\r\n") for l in bio if l.startswith(b">")]

	# l = [collections.Counter(e[:i] for e in descs).most_common(1) for i in range(3, 20)]
	def get_freq() -> List[Tuple[bytes, int]]:
		l = []
		for i in range(3, 120):
			# t = collections.Counter(e[:i] for e in descs if ord(b'|') not in e[:i]).most_common(2)
			t = [e for e in collections.Counter(e[:i] for e in descs).most_common() if b'|' not in e[0]]
			if len(t) == 0:
				continue
			if len(t) == 1:
				l.append(t[0])
				break
			[(_, c1), (_, c2), *_] = t
			if c1 > c2:
				l.append(t[0])
				break
		else:
			raise RuntimeError("cannot detect decoy prefix")
		descs2 = descs
		for ii in range(i + 1, 33):
			prev_prefix = l[-1][0]
			descs2 = [e for e in descs2 if e.startswith(prev_prefix) and len(e) >= ii and e[ii - 1] != ord(b'|')]
			mc = collections.Counter(e[:ii] for e in descs2).most_common(1)
			if len(mc) == 0:
				pass
			else:
				[mc0, ] = mc
				l.append(mc0)
		return l

	l = get_freq()
	counts = [e for _, e in l]

	for j in range(len(l) - 2):
		c0, c1, c2 = counts[j:j + 3]
		(current_prefix, c1_) = l[j + 1]
		assert c1 == c1_
		if current_prefix[-1:] == b'|':  # prefix should not have a '|' character
			if c0 != c1:
				raise RuntimeError("cannot detect decoy prefix")
			prefix_len = j
			break
		if c0 == c1 > c2 or \
				(c0 == c1 and current_prefix[-1:] == b'_'):  # if underscore seen, assume it is the last character of prefix
			prefix_len = j + 1
			break
	else:
		raise RuntimeError("cannot detect decoy prefix")

	(ret, count) = l[prefix_len]
	# print((count,count/len(descs)))
	if not 0.4 < count / len(descs) < 0.6:
		return None, ret
	return ret.decode("ascii")


def detect_decoy_prefix(bio: IO[bytes]) -> str:
	descs = [l[1:].rstrip(b"\r\n") for l in bio if l.startswith(b">")]
	num_of_seqs = len(descs)
	a = []
	# descs.sort() #####
	for i in range(2, 120):
		# t = [e for e in collections.Counter(e[:i] for e in descs).most_common(2) if b'|' not in e[0]]
		t = collections.Counter(e[:i] for e in descs).most_common(2)
		# t = sorted(
		# 	[(prefix, len(list(l)))
		# 	 for prefix, l in itertools.groupby(descs, operator.itemgetter(slice(None,i)))]
		# 	, key=operator.itemgetter(1), reverse=True)[:2]
		if len(t) == 0:
			return None
		prop = t[0][1] / num_of_seqs
		# print(prop);		print(t)
		if 0.49 < prop < 0.51:
			a.extend(t)
		# a.extend([(seq,count) for seq,count in t if 0.49 < count / num_of_seqs < 0.51 and b'|' not in seq])
		if prop < 0.49:
			break
	else:
		return None
	a2 = [(seq, count) for seq, count in a if 0.49 < count / num_of_seqs < 0.51 and b'|' not in seq]
	# print(a);	print(a2)
	if len(a2) == 0:
		return None
	prefixes = [seq for seq, count in a2]
	idx = prefixes[-1].find(b'_')
	if idx == -1:
		return prefixes[-1].decode("ascii")
	p = prefixes[-1][:idx + 1]
	return (p if p in prefixes else prefixes[-1]).decode("ascii")


if __name__ == "__main__":
	if not len(sys.argv) == 2:
		print("needs fasta filename as the program argument")
		sys.exit(1)
	import pathlib

	with pathlib.Path(sys.argv[1]).open('rb') as f:
		print(detect_decoy_prefix(f))
