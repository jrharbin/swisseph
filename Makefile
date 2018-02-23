CLIBS=swe
SOURCES=swe.mli swe.ml swedates.ml swe_ephem.mli swe_ephem.ml
PACKS=ctypes ctypes.foreign ANSITerminal core rendertable
THREADS=yes
RESULT=swe
-include OCamlMakefile

install: bcl ncl
	ocamlfind install swisseph META swe.a swe.cmi swe_ephem.cmi swe.cma -optional swe.cmxa swe.cmxs

remove:
	ocamlfind remove swisseph
