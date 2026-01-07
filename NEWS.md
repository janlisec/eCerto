# eCerto 0.8.10 [07.01.2026]
* fix error in LTS modul (error upon new value entry)

# eCerto 0.8.9 [18.11.2025]
* set up a 'devel' branch on GitHub to keep 'main' branch non-failing

# eCerto 0.8.8 [06.11.2025]
* DRMD v0.3.0 now supported
* XML validation implemented
* DRMD schema incorporated as external data
* DRMD table visualization improved for idx column

# eCerto 0.8.7 [03.11.2025]
* Docker container creation incorporated in the GitHub CI

# eCerto 0.8.5
* skipping a test for markdown rendering the help files on CRAN to avoid an 
  markdown related error

# eCerto 0.8.4
* function adjusted to comply with current DRMD XML schema definition

# eCerto 0.8.3
* Tab.C3 can be exported to a DRMD XML file
* Tab.D2 indicates relevant XML node information (for selected measurement node)
  in header
* Tab.D1 shows index path for each value allowing to selectively edit nodes
  with similar name path
* various (internal) functions to work with XML data (flatten/extend lists),
  setup DRMD specific structure, etc.

# eCerto 0.8.2

* analyte parameter UI moved to Tab.C3
* report buttons moved to Tab.C3
* all card items in C module are collapsible now
* Fig.C1 can be moved between layout columns to better deal with CRMs containing 
  many (>15) analytes
* D module got improved layout

# eCerto 0.8.1

* New module DRMD (Digital Reference Material Document)
* allows to upload a XML file in DRMD format (currently still under development)
* presents administrative data and measurement data from the XML in 2 tables
* allows to modify administrative data
* allows to write modified XML to disc
* help files for DRMD module initialized
* report file for DRMD module initialized
* required 2 new dependencies (`dplyr` and `xml2`)

# eCerto 0.7.4

* Added a `NEWS.md` file to start tracking package changes.
* Added a static badge to the `README.md` providing the link to the LiveApp 
  of the current GitHub version.