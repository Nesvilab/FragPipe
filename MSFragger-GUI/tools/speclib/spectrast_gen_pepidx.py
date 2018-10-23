#!/usr/bin/env python
"""
Copyright (c) 2012, ETH Zurich, Insitute of Molecular Systems Biology, 
George Rosenberger
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ETH ZURICH, INSTITUTE OF MOLECULAR
SYSTEMS BIOLOGY, GEORGE ROSENBERGER BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
from __future__ import print_function
import sys
import os
import csv
import getopt
import scipy
from numpy import *
from scipy import stats
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
#import profile 

#from chauvenet import *
"""
Copyright (c) 2010-2012, Kapteyn Astronomical Institute, University
of Groningen. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the Kapteyn Astronomical Institute nor the names
      of its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

# Chauvenet from http://www.astro.rug.nl/software/kapteyn/plot_directive/EXAMPLES/kmpfit_hubblefit.py
from scipy.special import erfc
import numpy

def chauvenet(x, y, mean=None, stdv=None):
   #-----------------------------------------------------------
   # Input:  NumPy arrays x, y that represent measured data
   #         A single value of a mean can be entered or a
   #         sequence of means with the same length as
   #         the arrays x and y. In the latter case, the
   #         mean could be a model with best-fit parameters.
   # Output: It returns a boolean array as filter.
   #         The False values correspond to the array elements
   #         that should be excluded
   #
   # First standardize the distances to the mean value
   # d = abs(y-mean)/stdv so that this distance is in terms
   # of the standard deviation.
   # Then the  CDF of the normal distr. is given by
   # phi = 1/2+1/2*erf(d/sqrt(2))
   # Note that we want the CDF from -inf to -d and from d to +inf.
   # Note also erf(-d) = -erf(d).
   # Then the threshold probability = 1-erf(d/sqrt(2))
   # Note, the complementary error function erfc(d) = 1-erf(d)
   # So the threshold probability pt = erfc(d/sqrt(2))
   # If d becomes bigger, this probability becomes smaller.
   # If this probability (to obtain a deviation from the mean)
   # becomes smaller than 1/(2N) than we reject the data point
   # as valid. In this function we return an array with booleans
   # to set the accepted values.
   #
   # use of filter:
   # xf = x[filter]; yf = y[filter]
   # xr = x[~filter]; yr = y[~filter]
   # xf, yf are cleaned versions of x and y and with the valid entries
   # xr, yr are the rejected values from array x and y
   #-----------------------------------------------------------
   if mean is None:
      mean = y.mean()           # Mean of incoming array y
   if stdv is None:
      stdv = y.std()            # Its standard deviation
   N = len(y)                   # Lenght of incoming arrays
   criterion = 1.0/(2*N)        # Chauvenet's criterion
   d = abs(y-mean)/stdv         # Distance of a value to mean in stdv's
   d /= 2.0**0.5                # The left and right tail threshold values
   prob = erfc(d)               # Area normal dist.
   filter = prob >= criterion   # The 'accept' filter array with booleans
   return filter                # Use boolean array outside this function




def lmedian(valarr):
  vals = sorted(valarr)
  if len(vals) % 2 == 1:
    return vals[(len(vals) + 1) // 2 - 1]
  else:
    return vals[len(vals) // 2 - 1]

def all_indices(value, qlist):
  indices = []
  idx = -1
  while True:
    try:
      idx = qlist.index(value, idx+1)
      indices.append(idx)
    except ValueError:
      break
  return indices

class sptxtio(object):
  def __init__(self):
    self.header = []
    self.spectra = []
    self.blocks = []
    self.spectrablocks = []
    self.rtkit = {}
    self.rt_all = {}
    self.prob_all = {}
    self.rt = {}
    self.rt_run = {}
    self.irt = {}
    self.irt_merged = {}
    self.a = {}
    self.b = {}
    self.rsq = {}
    self.spectrum_block_map = {}
  
  def input(self,file,precursorlevel,spectralevel):
    try:
      sptxt_infile = open(file, 'r')
    except IOError:
      print(file, "not readable")
    
    sptxt_header = []
    sptxt_block = []
    for sptxt_line in sptxt_infile:
      if sptxt_line[0] == "#":
        sptxt_header.append(sptxt_line)
      else:
        sptxt_block.append(sptxt_line)
        if sptxt_line == "\n":
          if precursorlevel:
            unique_identifier = sptxt_block[0].split("Name: ")[1].split("\n")[0]
          elif spectralevel:
            unique_identifier = sptxt_block[6].split("RawSpectrum=")[1].split(" ")[0]
          else:
            unique_identifier = sptxt_block[0].split("Name: ")[1].split("/")[0]
          sequence = sptxt_block[0].split("Name: ")[1].split("/")[0]

          block = blockio(sptxt_block[1].split("LibID: ")[1].split("\n")[0],
                            unique_identifier, sequence,
                            sptxt_block[0].split("Name: ")[1].split("/")[1].split("\n")[0],
                            sptxt_block[6].split("Mods=")[1].split(" ")[0],
                            sptxt_block[6].split("RawSpectrum=")[1].split(".")[0],
                            float(sptxt_block[6].split("RetentionTime=")[1].split(",")[0]),
                            float(sptxt_block[6].split("Prob=")[1].split(" ")[0]),
                          sptxt_block)
          self.push(block)
          sptxt_block = []
    sptxt_infile.close()
    self.pushheader(sptxt_header)
  
  def output(self,file):
    try:
      sptxt_outfile = open(file, 'w')
      sptxt_outfile.writelines(self.header)
      newblocks = []
      for block in self.blocks:
        block.binindex = sptxt_outfile.tell()
        sptxt_outfile.writelines(block.block)
        newblocks.append(block)
      self.blocks = newblocks
    except IOError:
      print(file, "not readable")
    sptxt_outfile.close()

  def report(self,file):
    try:
      report_outfile = open(file, 'w')
      report = csv.writer(report_outfile, delimiter=',')
      report.writerow(['peptide','rt','irt','rt_lmedian','rt_mean','rt_sd','irt_lmedian','irt_mean','irt_sd','rt_run_lmedian','rt_run_mean','rt_run_sd'])
      ind = {}
      for spectrum in self.irt:
        for peptide in self.irt[spectrum]:
          if peptide not in ind:
            ind[peptide] = []
          ind[peptide].append(spectrum)

      for peptide in ind:
        rt = []
        irt = []
        rt_run_median = []
        rt_run_mean = []
        rt_run_sd = []
        for spectrum in ind[peptide]:
          rt.append(self.rt[spectrum][peptide])
          irt.append(self.irt[spectrum][peptide])
          rt_run_median.append(spectrum+":"+str(lmedian(self.rt_run[spectrum][peptide])))
          rt_run_mean.append(spectrum+":"+str(mean(self.rt_run[spectrum][peptide])))
          rt_run_sd.append(spectrum+":"+str(std(self.rt_run[spectrum][peptide])))
        report.writerow([peptide,lmedian(rt),self.irt_merged[peptide],lmedian(rt),mean(rt),std(rt),lmedian(irt),mean(irt),std(irt),";".join(rt_run_median),";".join(rt_run_mean),";".join(rt_run_sd)])
    except IOError:
      print(file, "not readable")
    report_outfile.close()

  def push(self,block):
    self.spectra.append(block.rawspectrum)
    self.spectra = list(set(self.spectra))
    self.blocks.append(block)

    if block.rawspectrum not in self.spectrablocks:
      self.spectrablocks.append(block.rawspectrum)
      self.rtkit[block.rawspectrum] = []
      self.rt_all[block.rawspectrum] = {}
      self.prob_all[block.rawspectrum] = {}
      self.rt[block.rawspectrum] = {}
      self.rt_run[block.rawspectrum] = {}
      self.irt[block.rawspectrum] = {}
      self.a[block.rawspectrum] = float()
      self.b[block.rawspectrum] = float()
      self.rsq[block.rawspectrum] = float()
      self.spectrum_block_map[block.rawspectrum] = {}

    self.spectrum_block_map[block.rawspectrum][block.peptide] = block
    
    if block.peptide not in self.rt_all[block.rawspectrum]:
      self.rt_all[block.rawspectrum][block.peptide] = []
      self.prob_all[block.rawspectrum][block.peptide] = []
    self.rt_all[block.rawspectrum][block.peptide].append(block.rt)
    self.prob_all[block.rawspectrum][block.peptide].append(block.prob)
  
  def pushheader(self,header):
    self.header = header
  
  def merge(self,rmout):
    for rawspectrum in self.rt_all:
      for peptide in self.rt_all[rawspectrum]:
        rt = []
        for idx in all_indices(sorted(self.prob_all[rawspectrum][peptide], reverse=True)[0],self.prob_all[rawspectrum][peptide]):
          rt.append(self.rt_all[rawspectrum][peptide][idx])

        if rmout and len(rt) > 2:
          self.rt[rawspectrum][peptide] = lmedian(array(rt)[chauvenet(array(rt),array(rt))])
          self.rt_run[rawspectrum][peptide] = array(rt)[chauvenet(array(rt),array(rt))]
        else:
          self.rt[rawspectrum][peptide] = lmedian(rt)
          self.rt_run[rawspectrum][peptide] = rt
  
  def calibrate(self,rtkit,outliers,surrogates,linregs,rsq_threshold):
    missingirt = []
    for rawspectrum in self.spectra:
      rt_calibration = []
      irt_calibration = []
      print("RT peptides used per run:")
      print("run\tpeptide\trt\tirt")
      for peptide in self.rt[rawspectrum]:
        peptide_sequence = self.spectrum_block_map[rawspectrum][peptide].sequence
        if peptide_sequence in rtkit:
          if rawspectrum in outliers:
            if peptide_sequence not in outliers[rawspectrum]:
              rt_calibration.append(self.rt[rawspectrum][peptide])
              irt_calibration.append(rtkit[peptide_sequence])
              print(rawspectrum + "\t" + peptide_sequence + "\t" + str(self.rt[rawspectrum][peptide]) + "\t" + str(rtkit[peptide_sequence]))
          else:
            rt_calibration.append(self.rt[rawspectrum][peptide])
            irt_calibration.append(rtkit[peptide_sequence])
            print(rawspectrum + "\t" + peptide_sequence + "\t" + str(self.rt[rawspectrum][peptide]) + "\t" + str(rtkit[peptide_sequence]))

      if len(rt_calibration) < 2:
        missingirt.append(rawspectrum)

      if len(rt_calibration) >= 2:
        if rawspectrum in linregs:
          print("Replacing iRT normalization of run " + rawspectrum + " with c: " + str(linregs[rawspectrum]['b']) + " m: " + str(linregs[rawspectrum]['a']) + ".")
          self.a[rawspectrum] = linregs[rawspectrum]['a']
          self.b[rawspectrum] = linregs[rawspectrum]['b']
          self.rsq[rawspectrum] = 1.0
        else:
          slope, intercept, r_value, p_value, std_err = stats.linregress(rt_calibration,irt_calibration)
          (self.a[rawspectrum], self.b[rawspectrum]) = (slope, intercept)
          self.rsq[rawspectrum] = r_value**2
          plt.figure()
          plt.title(rawspectrum + " (c: " + str(self.b[rawspectrum]) + ", m: " + str(self.a[rawspectrum]) + ")")
          fit_fn = scipy.poly1d((self.a[rawspectrum],self.b[rawspectrum]))
          plt.plot(rt_calibration,irt_calibration, 'b.',rt_calibration,fit_fn(rt_calibration),'-r')
          plt.savefig(rawspectrum+'.png')

    for host in surrogates:
      print("Replacing iRT normalization of run " + host + " with " + surrogates[host] + ".")
      self.a[host] = self.a[surrogates[host]]
      self.b[host] = self.b[surrogates[host]]
      self.rsq[host] = self.rsq[surrogates[host]]
      missingirt = [a for a in missingirt if a != host]

    if len(missingirt) > 0:
      print("Did you search for the true sequences?")
      print("Did you use a non-consensus and without best replicates summarization spectral library?")
      print("Did you add the Biognosys RT-kit to all of your samples?")
      print("The following runs don't contain peptides from the Biognosys RT-kit:",missingirt)
      raise Exception("Error: At least one of your runs doesn't contain any peptides from the Biognosys RT-kit!")

    for rawspectrum in self.spectra:
      if self.rsq[rawspectrum] < rsq_threshold:
        raise Exception("Error: R-squared " + str(self.rsq[rawspectrum]) + " of run " + rawspectrum + " is below the threshold of " + str(rsq_threshold) + ".")

  def transform(self,rmout):
    irt = {}
    for rawspectrum in self.rt:
      for peptide in self.rt[rawspectrum]:
        self.irt[rawspectrum][peptide] = scipy.polyval([self.a[rawspectrum],self.b[rawspectrum]],self.rt[rawspectrum][peptide])
        if peptide not in irt:
          irt[peptide] = []
        irt[peptide].append(self.irt[rawspectrum][peptide])

    for peptide in irt:
      if len(irt) == 1:
        self.irt_merged[peptide] = round(irt[peptide][0],5)
      else:
        if rmout and len(irt) > 2:
          self.irt_merged[peptide] = round(lmedian(array(irt[peptide])[invert(chauvenet(array(irt[peptide]),array(irt[peptide])))]),5)
        else:
          self.irt_merged[peptide] = round(lmedian(irt[peptide]),5)
 
    for i in range(0,len(self.blocks)):
      self.blocks[i].replace(self.irt_merged[self.blocks[i].peptide])

class blockio(object):
  def __init__(self,libid,peptide,sequence,charge,mods,rawspectrum,rt,prob,block):
    self.libid = libid
    self.peptide = peptide
    self.sequence = sequence
    self.charge = charge
    self.mods = mods
    self.binindex = -1
    self.rawspectrum = rawspectrum
    self.rt = rt
    self.prob = prob
    self.irt = float()
    self.block = block
  
  def replace(self,irt):
    self.irt = irt
    block_new = []
    for line in self.block[:6]:
      block_new.append(line)
    
    block_new.append(self.block[6][:self.block[6].find("RetentionTime=")+len("RetentionTime=")]+str(self.irt)+","+str(self.irt)+","+str(self.irt)+self.block[6][self.block[6][self.block[6].find("RetentionTime="):].find(" ")+self.block[6].find("RetentionTime="):])
  
    for line in self.block[7:]:
      block_new.append(line)
    self.block = block_new

class pepidxio(object):
  def __init__(self,blocks):
    self.blocks = blocks
    self.pepindex = {}
    
  def pepind(self):
    for block in self.blocks:
      id = block.sequence + "\t" + block.charge + "|" + block.mods + "|\t"
      if id not in self.pepindex:
        self.pepindex[id] = []
      self.pepindex[id].append(block.binindex)
    
  def output(self,pepidx_outfile):
    try:
      pepidx_out = open(pepidx_outfile, 'w')
      for key in self.pepindex:
        line = key + ' '.join(str(x) for x in self.pepindex[key]) + "\n"
        pepidx_out.write(line)
    except IOError:
      print(pepidx_outfile, "not readable")
    pepidx_out.close()

# MAIN
def main(argv):
  rtkit = {'LGGNEQVTR': -28.3083,'GAGSSEPVTGLDAK': 0.227424,'VEATFGVDESNAK': 13.1078,'YILAGVENSK': 22.3798,'TPVISGGPYEYR': 28.9999,'TPVITGAPYEYR': 33.6311,'DGLDAASYYAPVR': 43.2819,'ADVTPADFSEWSK': 54.969,'GTFIIDPGGVIR': 71.3819,'GTFIIDPAAVIR': 86.7152,'LFLQFGAQGSPFLK':98.0897}

  splib_in = False
  splib_out = False
  outliers={}
  surrogates={}
  linregs={}
  rmout = False
  precursorlevel = False
  spectralevel = False
  report = False
  rsq_threshold = 0.95

  help = False
  try:
    opts, args = getopt.getopt(argv, "i:o:k:apre:s:l:t:",["in=","out=","kit=","applychauvenet","spectralevel", "precursorlevel","report","exclude=","surrogate=","linearregressions=","rsq_threshold="])
  except getopt.GetoptError:
    help = True
    opts =  ( ("",""),)

  for opt, arg in opts:
    if opt in ("-i","--in"):
      splib_in = arg
    elif opt in ("-o","--out"):
      splib_out = arg
      pepidx_out = os.path.splitext(arg)[0] + '.pepidx'
      report_out = os.path.splitext(arg)[0] + '.csv'
    elif opt in ("-a","--applychauvenet"):
      rmout = True
    elif opt in ("-k","--kit"):
      rtkit = {}
      for peptide in arg.split(","):
        rtkit[peptide.split(":")[0]] = float(peptide.split(":")[1])
    elif opt in ("-p","--precursorlevel"):
      precursorlevel = True
    elif opt in ("--spectralevel"):
      spectralevel = True
    elif opt in ("-r","--report"):
      report = True
    elif opt in ("-e","--exclude"):
      for outlier in arg.split(","):
        if outlier.split(":")[0] not in outliers:
          outliers[outlier.split(":")[0]] = []
        outliers[outlier.split(":")[0]].append(outlier.split(":")[1])
    elif opt in ("-s","--surrogate"):
      for surrogate in arg.split(","):
        surrogates[surrogate.split(":")[0]] = surrogate.split(":")[1]
    elif opt in ("-l","--linearregression"):
      for linreg in arg.split(","):
        linregs[linreg.split(":")[0]] = {}
        linregs[linreg.split(":")[0]]['a'] = float(linreg.split(":")[1].split("/")[1])
        linregs[linreg.split(":")[0]]['b'] = float(linreg.split(":")[1].split("/")[0])
    elif opt in ("-t","--rsq_threshold"):
        rsq_threshold = float(arg)

  if help or not splib_in or not splib_out:
    print("SpectraST RT Normalizer")
    print("---------------------------------------------------------------------------------------------")
    print("Usage:     spectrast2spectrast_irt.py -i non_consensus_library.[splib/sptxt] -o non_consensus_library_irt.splib")
    print("Input:     SpectraST non_consensus_library.splib in txt format")
    print("Output:    SpectraST non_consensus_library_irt.[splib/pepidx] and regression plots for all runs.")
    print("Argument:  -i [--in]: input file")
    print("           -o [--out]: output file")
    print("           (optional) -k [--kit]: specifiy RT-kit [LGGNEQVTR:-28.3083,GAGSSEPVTGLDAK:0.227424,VEATFGVDESNAK:13.1078,YILAGVENSK:22.3798,TPVISGGPYEYR:28.9999,TPVITGAPYEYR:33.6311,DGLDAASYYAPVR:43.2819,ADVTPADFSEWSK:54.969,GTFIIDPGGVIR:71.3819,GTFIIDPAAVIR:86.7152,LFLQFGAQGSPFLK:98.0897]")
    print("           (optional) -a [--applychauvenet]: should Chavenet's criterion be used to exclude outliers?")
    print("           (optional) -p [--precursorlevel]: should precursors instead of peptides be used for grouping?")
    print("           (optional)    [--spectralevel]: do not merge or group any peptides or precursors (use raw spectra)")
    print("           (optional) -r [--report]: should a csv report be written?")
    print("           (optional) -e [--exclude]: specify peptides from the RT-kit to exclude [run_id1:LGGNEQVTR,run_id2:GAGSSEPVTGLDAK]")
    print("           (optional) -s [--surrogate]: specify surrogate calibrations [broken_run_id1:working_run_id2]")
    print("           (optional) -l [--linearregression]: specify surrogate linear regressions (first number: c, second number: m) [broken_run_id1:1/3]")
    print("           (optional) -t [--rsq_threshold]: specify r-squared threshold to accept linear regression [0.95]")
    print("Important: The splib need to be in txt format!")
    print("           spectrast -c_BIN! -cNnon_consensus.txt non_consensus.bin.splib")
    print("           All runs in your library further need to contain the Biognosys RT-kit peptides.")
    print("Contact:   George Rosenberger <rosenberger@imsb.biol.ethz.ch>")
    sys.exit()

  # splib containing all spectra
  sptxt_all = sptxtio() # create object
  sptxt_all.input(splib_in, precursorlevel, spectralevel) # read sptxt
  sptxt_all.merge(rmout) # merge retention times of same peptides in each run individually
  # sptxt_all.calibrate(rtkit,outliers,surrogates,linregs,rsq_threshold) # calibrate using the retention kit
  # sptxt_all.transform(rmout) # transform retention times to iRT
  sptxt_all.output(splib_out) # write sptxt

  # write pepidx
  pepidx = pepidxio(sptxt_all.blocks)
  pepidx.pepind()
  pepidx.output(pepidx_out)

  # write report
  if report:
    sptxt_all.report(report_out)

#profile.run('main()')
if __name__ == "__main__":
  main(sys.argv[1:])
