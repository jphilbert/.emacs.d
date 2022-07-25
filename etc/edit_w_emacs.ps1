#USE: powershell -windowstyle hidden -file "<PATH>edit_w_emacs.ps1" "%1"
$emacs_path = "C:\EMACS\bin\"

$file = $args -replace "\\","/"
$command = "$($emacs_path)emacsclientw.exe"
$arglist = "-a=""$($emacs_path)runemacs.exe"" -n -e ""(find-file-other-frame \""$file\"")"""

Start-Process -NoNewWindow -FilePath $command -ArgumentList $arglist 