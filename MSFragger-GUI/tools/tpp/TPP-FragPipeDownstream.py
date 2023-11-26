"""
Author: Carolina Rojas Ramirez
Date: Sep 27th, 2023
Script to add 1DTPP and 2DTPP analysis in FragPipe Downstream Tools
"""

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~1DTPP code~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import pandas as pd
import sys
import numpy as np
import pickle
import os
import pandas
import re

#Start command line adaptation
class Parameters(object):
    """
    container for parameters
    """
    preoneDTPP_bool:str
    fragpipeoutput_path: str
    oneDTPP_bool: bool
    rhome_path:str
    #configTPPR_path: str
    twoDTPP_bool: bool
    fasfile_path: str
    tmtifile_path: str

    def __init__(self, twoDTPPboo, mainfolderval, rlocalinstall = None, fasfileval = None, tmtifileval = None):

        #Used the print statements below to fix a postional argument conundrum
        # print(type(fasfileval))
        # print(type(tmtifileval))
        # print(fasfileval)
        # print(tmtifileval)
        # print(type(rlocalinstall))
        # print(rlocalinstall)


        # self.oneDTPPR_bool = oneDTPPbooR.lower().strip() == 'true'
        # self.oneDTPP_bool = oneDTPPboo.lower().strip() == 'true'
        self.twoDTPP_bool =  twoDTPPboo.lower().strip() == 'true'
        self.fragpipeoutput_path = mainfolderval
        self.rhome_path = rlocalinstall
        self.fasfile_path =  fasfileval
        self.tmtifile_path = tmtifileval
        self.check_file_paths()

    def check_file_paths(self):
        """
        make sure the passed files exist and give nice error messages if not
        """
        # if self.oneDTPP_bool and not os.path.exists(self.rhome_path):
        #     print('Error: R path {} does not exist! Stopping analysis.'.format(self.rhome_path))
        #     sys.exit(1)
        if self.twoDTPP_bool and not os.path.exists(self.fasfile_path):
            print('Error: database file {} does not exist! Stopping analysis.'.format(self.fasfile_path))
            sys.exit(1)
        if self.twoDTPP_bool and not os.path.exists(self.tmtifile_path):
            print('Error: TMTI file {} does not exist! Stopping analysis.'.format(self.tmtifile_path))
            sys.exit(1)

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
        raise Exception('Python 3.2 or above is required')

    argv = sys.argv[1:]
    if len(argv) == 0:
        print('Example usage:')
        print('python3 TPP-FragPipeDownstream.py [1DTPP (TPP_R) bool] [1DTPP (TP-MAP) bool] [2DTPP bool] [FragPipe Output folder] [R location] [database fas file name (2DTPP analysis only)] [TMTI file name (2DTPP analysis only)]')
        sys.exit(0)

    params = Parameters(*argv)
    # if params.oneDTPPR_bool:
    #     oneDTPPR_analysis(params)
    # elif params.oneDTPP_bool:
    #     oneDTPP_analysis(params)
    if params.twoDTPP_bool:
        twoDTPP_analysis(params)
#End of command line adaptation
def renaming_dict(annotationfile):
    """
    Creates a dictionary to translate the TMT-labels to TPP-R labels
    @param annotationfile: TMT-I annotation file
    @return: a dictionary of labels relations
    """

    #Initiate dictionary to rename column headers
    outputrenamingdict = {"Protein ID": "Prot_ID"}

    #Dictionary to "translate temperature labels {key(TMTI):value (TPPR)}
    tpprlabelsdict = {"126": "rel_fc_126", "127N": 'rel_fc_127L', "127C": 'rel_fc_127H',"128N": 'rel_fc_128L',
                      "128C": 'rel_fc_128H', "129N": 'rel_fc_129L', "129C": 'rel_fc_129H',"130N": 'rel_fc_130L',
                      "130C": 'rel_fc_130H', "131N": 'rel_fc_131L'}

    tempvalsfortppr = {}

    #Open annotation file
    with open(annotationfile, 'r') as plexelabels:
        lines = list(plexelabels)
        for line in lines:
            line = line.rstrip('\n')
            splits = line.split("\t")

            #Obtain TMT label and experimental label (temperature_exptype-rep)
            tmtlabel = splits[0]
            explabel = splits[1]

            #Add the experimental label as key and tmt label as value
            outputrenamingdict[explabel] = tpprlabelsdict[tmtlabel]

            #Change formating to match the one needed by TPP-R config file
            configtmtlabel = tpprlabelsdict[tmtlabel].replace("rel_fc_","")
            explabelspl = explabel.split("_")
            configtemplabel = explabelspl[0]
            tempvalsfortppr[configtmtlabel] = float(configtemplabel)
    # print(f"tempvalsfortppr={tempvalsfortppr}")
    return outputrenamingdict, tempvalsfortppr

def fragpipe_to_TPPR(filepath, annotfile):
    """
    Function to transform protein.tsv to TPPR input ,txt file
    @param filepath: paht to protein.tsv file of interest
    @param annotfile: TMTI Annotation file
    @return: a dataframe
    """
    # print(f"protein.tsv file = {filepath} with {annotfile}")

    # How to rename column names
    oldcoltonewcol, tmt_to_tempdict = renaming_dict(annotfile)

    # Create pandas dataframe
    DT = pd.read_csv(filepath, sep='\t')

    # Copy original dataframe to prevent changes to the main DT
    outputDT = DT.copy()

    # Parse columns
    # print(outputDT.columns)

    # What columns to extract from the tmt-report abundance file. After "Indistinguishable Proteins" there are the
    # columns with abundance info
    thecolumns = list(outputDT.columns)
    referenceindex = thecolumns.index("Indistinguishable Proteins")
    outputcolumns = thecolumns[referenceindex + 1:]

    # Add as first column the one that contains the protein ID (the second column in the protein.tsv file)
    outputcolumns.insert(0, thecolumns[1])

    # Data frame with desired columns
    finalDT = outputDT.loc[:, outputcolumns]

    # Normalizing for highest temperature (needed by TPP-R)
    for i in range(1, 11):
        finalDT[outputcolumns[1:]] = finalDT[outputcolumns[1:]].div(finalDT[outputcolumns[1]], axis=0)

    # Convert Nan to zero
    finalDT = finalDT.fillna(0)

    # Rename to TPP-R compatible columns
    finalDT = finalDT.rename(columns=oldcoltonewcol)

    return finalDT, tmt_to_tempdict


def drscriptionparser(idxval, dataf):
    """
    Function to extract the info needed from protein.tsv files
    @param idxval: Index value to locate row needed for data extraction
    @param dataf: data frame to where extract the data
    @return: str, containing the information needed by TP-MAP
    """

    # Extracting each value from each column based on the index value
    proteindb = dataf.loc[idxval, "Protein"]
    proteindescript = dataf.loc[idxval, "Protein Description"]
    proteinorg = dataf.loc[idxval, "Organism"]
    proteingene = dataf.loc[idxval, "Gene"]
    proteinexist = dataf.loc[idxval, "Protein Existence"]

    descriptivestr = f"{proteindb} {proteindescript} OS={proteinorg} GN={proteingene} PE={proteinexist[0]}"

    return descriptivestr


def descriptionfile(file_path):
    """
    Function to extract the information needed from protein.tsv files
    @param file_path: path, to protein.tsv file
    @return: dict, keys:protein accession; values: description string
    """
    DT = pd.read_csv(file_path, sep='\t')
    descriptionout = {}
    for item in DT["Protein ID"]:
        descriptivestr = ''
        # print(f"item = {item}")
        condition = DT["Protein ID"] == item
        # print(f"condition = {condition}")
        indexnum = DT.index[condition]
        # print(f"indexum = {indexnum}")
        if len(indexnum) > 0:
            indexval = indexnum[0]
            descriptivestr = drscriptionparser(indexval, DT)
        descriptionout[item] = descriptivestr

    # descriptionoutdt = pd.DataFrame.from_dict(descriptionout,orient="index")
    return descriptionout



def runningTPPR(outputlocation, tpprconfiglocation, rhomelocation, mode = None):
    """
    Function to run R-script that normalized data using TPP_R
    @param outputlocation: where to save the TPP_R output
    @param tpprconfiglocation: where is the configuration file
    @return: void
    """

    print("Going to run R!")
    # Add R_HOME to environmental variables
    os.environ['R_HOME'] = rhomelocation

    # import rpy2 package
    from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage
    # print(os.environ)

    # Rcode

    RNormOnly = """
    #!/usr/bin/env Rscript

#Install pacakge
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("TPP", ask=FALSE)

options(echo=TRUE)

library(TPP)

tpprNormOneDTPP <- function(configurepath,resultPath){

  #Give data to TPPR
  trData <- tpptrImport(configTable = configurepath,  fcStr = "rel_fc_",idVar= "Prot_ID")

  #Normalize 1DTPP data 
  normResults <- tpptrNormalize(data=trData)

  #Where to save is set
  setwd(resultPath)

  #For each experiment extract normalized data
  for (x in normResults$normData){

    # Obtain data
    experimentname <- x@annotation["name"]
    normatrix <- normResults[["normData"]][[experimentname]]@assayData[["exprs"]]

    #Double matrix to data frame to save as csv file
    normdataframe <- as.data.frame(normatrix)
    outputname <- sprintf("TPP-TR_%s.csv",experimentname)
    write.csv(normdataframe, outputname)
  }

}
    """

    Full_workflow = """
    #!/usr/bin/env Rscript

#Install pacakge
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("TPP", ask=FALSE)

options(echo=TRUE)

library(TPP)

fullOneDTPP <- function(configurepath,resultPath){
  
  TPPTR_result <- analyzeTPPTR(configurepath,resultPath = resultPath, methods = "meltcurvefit",idVar= "Prot_ID")
  
  }

    """

    if mode == "NormOnly":
        tpprpack = SignatureTranslatedAnonymousPackage(RNormOnly, "tpprpack")
        tpprpack.tpprNormOneDTPP(tpprconfiglocation,outputlocation)
    else:
        tpprpack = SignatureTranslatedAnonymousPackage(Full_workflow, "tpprpack")
        tpprpack.fullOneDTPP(tpprconfiglocation,outputlocation)



def TPPR_to_TPMAP(froutputfolder):
    """
    Transforming TPP_R output into TP-MAP data
    @param froutputfolder: FragPipe output folder
    @return: void
    """

    # Dictionary to save output
    masterdict = {}
    # List to store the dictionaries obtained from the data output by TPP-R
    lsofdt = []

    # iteriating over files located in the FragPipe
    fpfiles = [x for x in os.listdir(froutputfolder)]

    # Find configuration file used by TPP-R
    configfileloc = os.path.join(froutputfolder, 'TPP-TR_config.xlsx')
    # Read configuration file
    configfile = pd.read_excel(configfileloc)

    # print(fpfiles)
    for csvfile in fpfiles:
        templabels = {"Unnamed: 0": "Accession"}

        # Find TPP-R output
        if csvfile.endswith("csv") and csvfile.startswith("TPP-TR"):
            print(csvfile)
            filepath = os.path.join(froutputfolder, csvfile)
            filedataframe = pd.read_csv(filepath, header=0)

            # Obtain experiment file from TPP-R output name
            experimentname = csvfile.strip(".csv")
            experimentname = experimentname.replace("TPP-TR_", "")

            #
            row = configfile[configfile["Experiment"] == experimentname].index.values

            # open pickle file containing the dictionary that has the protein info string
            with open(f"{experimentname}_descriptpmap.pickle", 'rb') as handle:
                descriptivedict = pickle.load(handle)

            # Join master dictionary and descriptive string dictionary
            masterdict = masterdict | descriptivedict

            # Itiriate over configuration file to obtain temperature information and make it TP-MAP compatible
            configfilecols = list(configfile.columns)
            # print(configfilecols)
            for col in configfilecols[4:-1]:
                # print(col)
                temp = configfile.loc[row, col]
                # print(temp)
                tempstring = np.array2string(temp.to_numpy()[0])
                templabels[f"rel_fc_{col}"] = f"Ref_{tempstring}_{experimentname.replace('_', '')}"

            # print(templabels)
            # Rename all columns in TPP-R output to be TP-MAP compatible
            finalDT = filedataframe.rename(columns=templabels)
            # Set accession column to be the index of the dataframe
            finalDT = finalDT.set_index("Accession")
            # print(finalDT)
            # Store the experiment dataframe in a list
            lsofdt.append(finalDT)

    # Concatanate TPP-R output obtained for each experiment, and which was stored in lsofdt
    concatDT = pd.concat(lsofdt, axis=1, verify_integrity=True)
    # replace nan with zero
    concatDT = concatDT.fillna(0)

    # print(concatDT.columns)
    # print(len(masterdict))

    # Add description column for TPMAP and add each descritpion string for each protein
    concatDT['Description'] = ''
    for idxval in concatDT.index:
        # print(idxval)
        # print(masterdict[idxval])
        concatDT.loc[idxval, 'Description'] = masterdict[idxval]

    # Save TP-MAP compatible file
    outname = os.path.join(froutputfolder, '1DTPP.tpmap.txt')
    concatDT.to_csv(outname, sep='\t', index=True)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2DTPP code~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


def fastaparser(filepathdb):
    """
    Function to obtain protein information string whole from FASTA file
    @param filepathdb: path, FASTA file location
    @return: dict, key:Protein Accession; value:string
    """

    #Intialize dictionary
    identifiers = {}

    # print(filepathdb)

    #Parse FASTA file
    with open(filepathdb, "r") as fh:
        for line in fh:
            #Only pay attention to protein headers
            if line.startswith(">"):

                #Look only at the string after "|"
                modregex = re.compile("\|", re.I)
                mo = modregex.finditer(line)
                proteinidindeces = []
                for thingy in mo:
                    proteinidindeces.append(thingy.span()[0])

                # print(proteinidindeces)

                #What is the protein id string
                keyproteinid = line[proteinidindeces[0]+1:proteinidindeces[1]]

                #Add protein id as key and whole description line as value
                identifiers[keyproteinid] = line

    return identifiers


def column_titlesformatter(colstoarrenge):
    """
    Organizing dataframe column headers for TP-MAP compatability
    @param colstoarrenge: what columns to change
    @return: tuple: list, dictionary
    """

    #Dictionary with the connections between old and new names
    renaming_dict = {}
    # print(colstoarrenge)

    #List if the column names to arrange
    arrangedcols = []
    arrangedcols.append(colstoarrenge[0])
    arrangedcols.append(colstoarrenge[-1])
    for item in colstoarrenge[1:-1]:
        arrangedcols.append(item)

    # print(arrangedcols)

    #Change TMTI temperature column headers to TP-MAP compatible ones
    for item in arrangedcols:
        newcolumntitle = ""

        if "_" in item:
            newcolumntitle = f"Ref_{item}"

        else:
            newcolumntitle = item

        renaming_dict[item] = newcolumntitle

    return arrangedcols, renaming_dict

def dataframecreator(proteindb, TMTIPath):
    """
    Putting together the output data frame to be input into TP-MAP
    @param proteindb: path to FASTA database
    @param TMTIPath: path to TMTI file
    @return: dataframe
    """

    fastafile = fastaparser(proteindb)
    abundanceDT = pandas.read_csv(TMTIPath, sep='\t')
    os.chdir(os.path.dirname(TMTIPath))

    # print(f"fastafile {fastafile}")
    #
    #Copy original to prevent hcanges to the main DT
    outputDT = abundanceDT.copy()
    #

    #What columns to extract from the tmt-report ratio file
    thecolumns = list(outputDT.columns)
    referenceindex = thecolumns.index("ReferenceIntensity")
    #
    # #First column and the ones that contain abundance information, excluding reference
    outputcolumns = thecolumns[referenceindex+1:]
    outputcolumns.insert(0, thecolumns[0])
    # print(outputcolumns)
    #
    #
    descriptionoutlist = []
    #Gathering description lines from FASTA file based on protein index from TMTI file
    for item in outputDT["Index"]:
        if item in fastafile:
            tpmapline = fastafile[item]
            tpmapline = tpmapline.strip("\n")
            tpmapline = tpmapline.replace(">","")
            descriptionoutlist.append(tpmapline)
    # print(f"descriptionoutlist = {descriptionoutlist}")

    #Selecting the columns to output and adding the description info for each protein
    finalDT = outputDT.loc[:, outputcolumns]
    finalDT["Description"] = descriptionoutlist

    #Format selected columns to be TP-MAP compatible
    oldcols = finalDT.columns.tolist()
    newcols, tpmapcols = column_titlesformatter(oldcols)
    finalDT = finalDT[newcols]

    # print(tpmapcols)

    #Rename Index header to accession
    tpmapcols["Index"] = "Accession"
    # print(tpmapcols)

    #Rename columns
    finalDT = finalDT.rename(columns=tpmapcols)

    #Replace nan with zero
    finalDT = finalDT.fillna(0)
    #
    return(finalDT)

def twoDTPP_analysis(argumentobj):
    """
        Function to run TPP-R and TP-MAP for 1DTPP analysis
        @param fastadatabsefile: Path to .fas file located inside the fragpipe output folder
        @param tmtifilepath: Path to TMTI file "ratio_protein_None.tsv"
        @return: void
        """
    print("inside fxn for 2DTPP")
    fastadatabsefile = argumentobj.fasfile_path
    print(fastadatabsefile)
    tmtifilepath = argumentobj.tmtifile_path
    tpmapDT = dataframecreator(fastadatabsefile, tmtifilepath)
    print(tpmapDT)

    #Create folder to save 2DTPP Results
    twodtpp_output = os.path.join(argumentobj.fragpipeoutput_path, "2DTPP")
    if not os.path.exists(twodtpp_output):
        os.mkdir(twodtpp_output)
    os.chdir(twodtpp_output)
    tpmapDT.to_csv(f"2DTPP.tpmap.txt", sep='\t', index=False)

#End of 2DTPP code
def oneDTPPR_analysis(argumentobj):
    """
    Function to run TPP-R and TP-MAP for 1DTPP analysis
    @param tppr_tpmapmainfolder: FragPipe Output folder path
    @param configfilename: location of the ready to input configuration file
    @return: void
    """

    dict_filesfor_TPPR = {}
    fragpipeoutputfolder = argumentobj.fragpipeoutput_path

    #Create TPP folder results
    tpprfolder = os.path.join(fragpipeoutputfolder,"1DTPP-TPPR")
    if not os.path.exists(tpprfolder):
        os.mkdir(tpprfolder)

    print(f"folder = {fragpipeoutputfolder}")
    diritems = [x for x in os.listdir(fragpipeoutputfolder)]
    # print(f"files = {diritems}")

    # For each experiments
    for item in diritems:
        print(f"item = {item}")

        # Continue if the item is not a folder
        if item.find(".") > -1:
            continue
        else:

            # Go to experiment file
            subdiritem = os.path.join(fragpipeoutputfolder, item)

            # print(f"subdiritem = {subdiritem}")

            subdirfiles = [x for x in os.listdir(subdiritem)]

            # print(f"subdirfiles = {subdirfiles}")

            for file in subdirfiles:

                # It is given the protein.tsv will be here, but also requite the annotation file

                targetproteinfile = os.path.join(subdiritem, "protein.tsv")

                if "annotation.txt" in file:
                    annotationfile = os.path.join(subdiritem, file)
                    resultDT, tmttempdict = fragpipe_to_TPPR(targetproteinfile, annotationfile)
                    tpmapdescriptions = descriptionfile(targetproteinfile)

                    outputname = os.path.basename(item)

                    if item in dict_filesfor_TPPR:
                        print("There cannot be two experiments with the same name and replicate number. Are there two annotation files under the same {} experiment folder?".format(item))
                        sys.exit(1)
                    else:
                        dict_filesfor_TPPR[item] = [item,tpprfolder,tmttempdict]

                    tpprinputfile = os.path.join(tpprfolder,f"{outputname}.txt")
                    resultDT.to_csv(tpprinputfile , sep='\t', index=False)

                    pickefilefortppr = os.path.join(tpprfolder,f"{os.path.basename(subdiritem)}_descriptpmap.pickle")
                    with open(pickefilefortppr, 'wb') as handle:
                        pickle.dump(tpmapdescriptions, handle, protocol=pickle.HIGHEST_PROTOCOL)


    print(f"dict_filesfor_TPPR = {dict_filesfor_TPPR}")
    #configfilepath = argumentobj.configTPPR_path
    rhomewherepath = argumentobj.rhome_path

    #Creating TPPR File
    configurefile = TPPR_configconstrcution(dict_filesfor_TPPR)

    configfilepath = os.path.join(tpprfolder,"TPP-TR_config.xlsx")
    configurefile.to_excel(configfilepath, index=False)


    #Runnig R package
    runningTPPR(tpprfolder,configfilepath,rhomewherepath, mode = "Complete")



def TPPR_configconstrcution(dictoffiles):
    """
    Function to create configuration file use by TPP-R
    @param filesls: List of the files to input into TPPR
    @return: dataframe converted from dict by pandas
    """


    #Initialize dictionary to turn to an excel file
    dictcongif = {"Experiment":[],"Condition":[],"Comparison1":[],"Comparison2":[],"126":[],"127L":[],"127H":[],
                  "128L":[],"128H":[],"129L":[],"129H":[],"130L":[],"130H":[],"131L":[],"Path":[]}

    #List of the dictionary keys
    #keysls = list(dictcongif.keys())

    for expfiles in dictoffiles:
        print(expfiles)
        #Only input experiment and location of input file to TPP-R
        fileitems = dictoffiles[expfiles]
        dictcongif['Experiment'].append(fileitems[0])
        condition_str = fileitems[0].split('_')
        dictcongif['Condition'].append(condition_str[0])
        if "1" in condition_str[1]:
            dictcongif["Comparison1"].append("x")
            dictcongif["Comparison2"].append("")
        else:
            dictcongif["Comparison2"].append("x")
            dictcongif["Comparison1"].append("")

        tempdict = fileitems[2]

        for tmtlabel in tempdict:
            dictcongif[tmtlabel] =tempdict[tmtlabel]


        fileloc = os.path.join(fileitems[1],f"{fileitems[0]}.txt")
        dictcongif['Path'].append(fileloc)

        #Place holders for user to fill
        # for key in keysls[4:-1]:
        #     dictcongif[key].append("")
    print(dictcongif)
    # Dataframe to dictionary
    df = pd.DataFrame.from_dict(dictcongif)

    return df

def oneDTPP_analysis(argumentobj):
    """
    Function to run TPP-R and TP-MAP for 1DTPP analysis
    @param tppr_tpmapmainfolder: FragPipe Output folder path
    @param configfilename: location of the ready to input configuration file
    @return: void
    """

    dict_filesfor_TPPR = {}
    fragpipeoutputfolder = argumentobj.fragpipeoutput_path

    #Create TPP folder results
    tppmapfolder = os.path.join(fragpipeoutputfolder,"1DTPP-TPMAP")
    if not os.path.exists(tppmapfolder):
        os.mkdir(tppmapfolder)
    print(f"folder = {fragpipeoutputfolder}")
    diritems = [x for x in os.listdir(fragpipeoutputfolder)]
    # print(f"files = {diritems}")

    # For each experiments
    for item in diritems:
        print(f"item = {item}")

        # Continue if the item is not a folder
        if item.find(".") > -1:
            continue
        else:

            # Go to experiment file
            subdiritem = os.path.join(fragpipeoutputfolder, item)

            # print(f"subdiritem = {subdiritem}")

            subdirfiles = [x for x in os.listdir(subdiritem)]

            # print(f"subdirfiles = {subdirfiles}")

            for file in subdirfiles:

                # It is given the protein.tsv will be here, but also requite the annotation file

                targetproteinfile = os.path.join(subdiritem, "protein.tsv")

                if "annotation.txt" in file:
                    annotationfile = os.path.join(subdiritem, file)
                    resultDT, tmttempdict = fragpipe_to_TPPR(targetproteinfile, annotationfile)
                    tpmapdescriptions = descriptionfile(targetproteinfile)

                    outputname = os.path.basename(item)

                    if item in dict_filesfor_TPPR:
                        print("There cannot bet two experiments with the same name and replicate number. Are there two annotation files under the same {} experiment folder?".format(item))
                        sys.exit(1)
                    else:
                        dict_filesfor_TPPR[item] = [item,tppmapfolder,tmttempdict]

                    tpprinputfile = os.path.join(tppmapfolder,f"{outputname}.txt")
                    resultDT.to_csv(tpprinputfile , sep='\t', index=False)

                    pickefilefortppr = os.path.join(tppmapfolder,f"{os.path.basename(subdiritem)}_descriptpmap.pickle")
                    with open(pickefilefortppr, 'wb') as handle:
                        pickle.dump(tpmapdescriptions, handle, protocol=pickle.HIGHEST_PROTOCOL)


    print(f"dict_filesfor_TPPR = {dict_filesfor_TPPR}")
    #configfilepath = argumentobj.configTPPR_path
    rhomewherepath = argumentobj.rhome_path

    #Creating TPPR File
    configurefile = TPPR_configconstrcution(dict_filesfor_TPPR)

    configfilepath = os.path.join(tppmapfolder,"TPP-TR_config.xlsx")
    configurefile.to_excel(configfilepath, index=False)

    # configfilepath = os.path.join(fragpipeoutputfolder, "TPP-TR_config.xlsx")

    #Runnig R package
    runningTPPR(tppmapfolder, configfilepath,rhomewherepath, mode = "NormOnly")

    #Convert TPP-R output into TP-MAP input
    TPPR_to_TPMAP(tppmapfolder)


if __name__ == "__main__":

    main()



