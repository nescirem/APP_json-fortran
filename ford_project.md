---
project: GTEA_Control
summary: GTEA_Control -- A Fortran JSON API for GTEA
author: Nescirem
src_dir: ./src
output_dir: ./doc
media_dir: ./media
exclude_dir: ./src/json_to_map_test
favicon: ./media/logo.jpg
project_github: https://github.com/nescirem/APP_json-fortran
project_download: https://github.com/nescirem/APP_json-fortran/releases/latest
github: https://github.com/nescirem
preprocessor: cpp
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         protected
         private
source: true
graph: true
sort: alpha
coloured_edges: true
extra_filetypes: .inc !
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
extra_mods: json_module:http://jacobwilliams.github.io/json-fortran/module/json_module.html
			iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            ifcore:https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-tracebackqq
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
