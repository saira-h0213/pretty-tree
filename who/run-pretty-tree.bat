@ECHO OFF
TITLE Pretty Tree Debug Edition
ECHO Running Pretty Tree...

START c:\progra~1\r\R-4.0.2\bin\Rscript.exe -e "source('ppi.R')"
ECHO Packages Installed Successfully

START c:\progra~1\r\R-4.0.2\bin\Rscript.exe -e "library(devtools); source_url('https://raw.githubusercontent.com/saira-h0213/pretty-tree/master/tgenerator.R')"

PAUSE
cmd /k
