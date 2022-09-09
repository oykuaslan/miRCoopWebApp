Dataset and Data Processing
====================================

.. _miRNATargets:
miRNA Targets
----------------------------
The experimentally validated miRNA targets were obtained from three different databases: 

  * `miRTarBase v8.0 <https://mirtarbase.cuhk.edu.cn/>`_
  * TarBase v7.0
  * miRecords v4.0

The experimentally validated miRNA targets were then intersected with TargetScan in order to obtain the binding regions.

Expression data and Pre-processing
----------------------------

For each TCGA cancer project, we retrieved mRNA expression levels, Fragments per Kilobase of transcript per Million mapped reads upper quartile normalisation (FPKM-UQ), from `GDC Data Portal <https://portal.gdc.cancer.gov/>`_ 
