Methods
=======

There can be three different modes of regulation in synergistic miRNA relationships with transcription factors.These situations can be:two miRNAs can regulate expression of a common gene by binding to two different TFs, one miRNA directly regulates the gene, while the second miRNA can bind to the TF and two miRNAs can bind to a single TF, regulating the expression of the gene.


Step 1: Identification of Candidate miRNA - mRNA - TF Interactions
--------
MiRNAs and TFs targeting the same gene were first found for cases where one miRNA directly targets the same gene through a TF jointly with another miRNA.Later, miRNAs targeting that TF were added to the list. Here, if the miRNA, which has a regulatory role via TF, interacts directly with the gene, these cases are not included.In cases where two miRNAs can regulate the expression of a common gene by binding to two different TFs, different TFs targeting the same gene have been identified first. Later, miRNAs targeting these TFs were found. Here, too, miRNA interactions, which have direct interactions with the gene, are not included. For cases where two miRNAs can regulate gene expression by binding to a single TF, a list of miRNAs targeting the same TF was created. Later this list was added to the genes targeted by TF.

It was obtained by crossing the experimentally detected miRNA-mRNA interactions at all steps with those obtained from TargetScan. Triplets in which miRNA pairs had an overlapping binding site on the target mRNA were excluded from the analysis. Gen-TF information was obtained from Dorothea in such a way that those with a confidence level of C and above were selected.

Step 2: Expression Filter
--------
The potential pairs obtained in the first step were filtered according to the expression of miRNA and mRNAs. Patient samples were divided into different subgroups in order to identify candidate triplets. First, for cases where one miRNA directly targets a common gene via a TF, the gene is expected to be down-regulated when the expression of both miRNAs increases, and up-regulated when the expression of both miRNAs decreases if the TF is an activator. If TF is a repressor, the gene is expected to be upregulated if the miRNA acting through TF is upregulated, and if the other miRNA is downregulated, the gene is expected to be downregulated if the miRNA acting through TF is downregulated.

Second, for cases where two miRNAs can regulate the expression of a common gene by binding to two different TFs, if activator and miRNAs are down in both TFs, the gene is likely to be upregulated, and if miRNAs are up, it is expected to be downregulated. If the repressor and miRNAs are upregulated in two TFs, the gene is expected to be upregulated, and if the miRNAs are downregulated, the gene is also expected to be downregulated. If one of the TFs is a repressor, the other an activator, and the miRNA binding to the repressor TF is upregulated and the other downregulated, the gene is expected to be upregulated, and for the opposite, the gene is expected to be downregulated.

Finally, for cases where two miRNAs can bind to a single TF and regulate the expression of the gene, if the TF repressor and miRNAs are upregulated, the gene is expected to be upregulated, and if the miRNAs are downregulated, the gene is also expected to be downregulated. If TF activator and miRNAs are upregulated, the gene is expected to be downregulated, and if miRNAs are downregulated, the gene is expected to be upregulated. In addition, TF is expected to be downregulated when miRNAs are upregulated and upregulated when miRNAs are downregulated.

When examining all cases, Wilcoxon rank-sum test was applied to test whether the difference in mRNA level between the first and second groups was significant. Cases where the null hypothesis was rejected (p-value 0.05) were kept as candidates for the next step.

Step 3:  Statistical Interaction Tests on RNA Expression Data
--------
We subject statistical interaction tests on RNA expression data for each potential synergy module that passes Step Two. First, miRNA1 and gene and miRNA2 and gene are pairwise independent of each other, for cases where one miRNA directly targets a common gene over a TF, and miRNA and TF acting on TF are pairwise dependent, and miRNA1-miRNA2- we want cases where the gene is mutually dependent triplets. Second, for cases where two miRNAs can regulate the expression of a common gene by binding to two different TFs, we want cases where miRNAs and TFs they target are interdependent, while the genes they target are independent of each other, and miRNA1-miRNA2-gene is triple-dependent. . Finally, for cases where two miRNAs can bind to a single TF to regulate the expression of the gene, we want cases where there is pairwise dependency between miRNAs and TF, pairwise independence between miRNAs and the gene, and where miRNA1-miRNA2-gene is dependent. Two Variable Testing for pairwise dependencies and kernel interaction tests for dependencies triplets are applied in tests for all cases.


