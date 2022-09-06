miRCoop v2
===================================

`miRCoop <https://ieeexplore.ieee.org/document/9311836>`_, is a method that finds potentially synergistic pairs through kernel interaction tests on matched gene expression levels of mRNAs and miRNAs. Now, we present an updated miRCoop pipeline, miRCoop v2, a faster version with a 30 speed up in runtime, which allowed us to apply it on 31 different TCGA cancers. The candidate cancer triplets, the synergistic miRNA pairs and their common targets,  are presented in `miRCoop web application <http://mircoop.sabanciuniv.edu>`_ to make our results available to the public. 

.. figure:: ../figures/web-app-general.png
  :scale: 100 %
  :align: center
  :alt: My Text

  Figure 1. General miRCoop and miRCoop Web Application pipeline

.. note::
  
  Required data files
  -------------------

  PyGenePlexus provides helper functions for setting up the necessary files in
  order to run the pipeline using a custom network. The required files are:

  #. ``NodeOrder_{your_net_name}.txt`` Network node ordering
      A text file with a single column containing all genes present in the
      network. The ordering of nodes in this file serves as the index map for the
      network feature data.
  #. ``Data_{feature_type}_{your_net_name}.npy`` A numpy array of the chosen network
      representation (rows are genes ordered by NodeOrder file, columns are features).

.. note::

   This project is under active development.

Contents
--------

.. toctree::
   :maxdepth: 1
   :caption: Methods & Dataset

   methods_dataset/methods
   methods_dataset/dataset

.. toctree::
   :maxdepth: 1
   :caption: miRCoop Web Application
   
   webapp/intro
   webapp/cancer_spec_analys
   webapp/pan_cancer_analys
   webapp/stats

   
.. toctree::
   :maxdepth: 1
   :caption: Appendix
   
   appendix/TCGA_projects
   appendix/glossary
   appendix/references
