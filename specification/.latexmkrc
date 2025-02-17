system('./scripts/latexmk-gitinfo2');
$pdf_mode = "1";
$pdflatex = "pdflatex -synctex=1 -file-line-error -interaction=nonstopmode";
$bibtex_use = "2";
$clean_ext = "bbl synctex.gz synctex.gz(busy)";
