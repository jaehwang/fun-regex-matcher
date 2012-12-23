TEX=$(wildcard *_report.tex)
PDF=${TEX:%.tex=%.pdf} report.pdf

all: ${PDF}

rerun = "(There were undefined references|Rerun to get (cross-references|the bars) right)"

report.tex: ${TEX:%_report.tex=%.tex}
	touch $@

%_report.tex: %.tex
	touch $@

%.pdf: %.tex preamble.tex macro.tex
	pdflatex ${@:%.pdf=%}
	(egrep -q $(rerun) ${@:%.pdf=%.log} && pdflatex ${@:%.pdf=%}) || true

grep3p.png:
	curl http://pdos.csail.mit.edu/~rsc/regexp-img/grep3p.png > $@
grep4p.png:
	curl http://pdos.csail.mit.edu/~rsc/regexp-img/grep4p.png > $@

result-compare.png:
	curl --no-sessionid "https://docs.google.com/spreadsheet/oimg?key=0Ah4i91oVLU-pdFlhOUlJMWs3SjE2MzZ2R1dBdVFEdnc&oid=3&zx=fmj3enqku403" > $@

result-single.png:
#	curl --no-sessionid "https://docs.google.com/spreadsheet/oimg?key=0Ah4i91oVLU-pdFlhOUlJMWs3SjE2MzZ2R1dBdVFEdnc&oid=4&zx=igofqmlk1ppt" > $@
	curl --no-sessionid "https://docs.google.com/spreadsheet/oimg?key=0Ah4i91oVLU-pdFlhOUlJMWs3SjE2MzZ2R1dBdVFEdnc&oid=4&zx=xqkcpek2yqyj" > $@

.PHONY: result-compare.png result-single.png

clean:
	rm -rf ${PDF} *.aux *.log *.nav *.out *.snm *.toc *.vrb
