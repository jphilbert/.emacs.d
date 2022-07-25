echo off
set file=%~1
set file=%file:\=/%
set emacs_path=C:\EMACS\bin\

%emacs_path%emacsclientw.exe -a="%emacs_path%runemacs.exe" -n -e "(find-file-other-frame \"%file%\")"