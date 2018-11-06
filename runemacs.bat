@echo off

call activate python27

set HOME=C:\Users\%USERNAME%
set PATH="C:\Users\Jonas\AppData\Local\Pandoc";"C:\Program Files\MiKTeX 2.9\miktex\bin\x64";%PATH%
"C:\Program Files (x86)\emacs\bin\runemacs.exe"
