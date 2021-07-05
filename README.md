# PsyCoP
 
The platform for systematic cognitive and behavioral profiling, short PsyCoP, was published with a pilot study here:
https://www.frontiersin.org/articles/10.3389/fnbeh.2020.618180/full.

It consists of series of behavioral experiments and an analysis pipeline. 

With our test battery, we present a generalizable approach for improving the predictive validity and translatability of psychiatric mouse models. The battery
consists of a diverse panel of well-established behavioral tests. Analysis according to behavioral traits and domains is R script based and automated.

The analysis pipeline can be found on this repository. Most of the code comes in the form of a FlowR bundle, which can be opened and read in FlowR.
For instructions how to install FlowR and the resources needed to get it running visit: http://www.xbehavior.com/flowr-for-you/.

Additionally, there is a number of complementary scripts available, which comprise the more sophisticated parts of the downstream analysis, as they are the subject
to regular change. However, some elements of the script have already been implemented in FlowR as part of the bundle and some more functions will follow.

Together with the implementations of FlowR, our scripts enable users to perform group comparisons of factorially designed experimental groups (specifically, 2x2 
in our case) as well as more complex dimension-reduction analyzes, i.e. Canonical Discriminant Analysis and Principal Component Analysis. They also provide
several heatmap visualization strategies in order to get an intuitive overview of behavioral profiles.

To get an example of the possibilities of our approach, you may have a look at the aforementioned publication to check the experimental design and possible working
results of our tools.
