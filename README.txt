This is my second implementation of the Compiling Control technique pioneered by Bruynooghe and De Schreye.
This implementation supersedes the one mentioned in the Compiling Control papers published between 2014 and 2016.

This package is organized into the following collections:
- cclp-common-data: data structures that can be used in any other collection
- cclp-syntax: syntax common to multiple project languages
- alpha-gamma: a language extension for convenient notation of the structures in cclp-common-data
- cclp-common: functionality needed for the remaining collections
- analysistree: a language for efficiently writing down analysis trees (independent from gengraph)
- gengraph: a language for efficiently writing down generational graphs (independent from analysistree)
- cclp-analysis: most of the mechanisms in Compiling Control, use any of the aforementioned collections
