'USE: "wscript.exe \"%~d0%~p0edit_w_emacs.vbs\" \"%%1\""
'i.e. wscript.exe "<PATH>edit_w_emacs.vbs" "%1"
 
file=Replace(WScript.Arguments(0),"\","/")
emacs_path="C:\EMACS\bin\"

CreateObject("WScript.Shell").Run """" & emacs_path & "emacsclientw.exe" & """" &  " -a=" & """" & emacs_path & "runemacs.exe" & """" & " -n -e " & """" & "(find-file-other-frame \" & """" & file & "\" & """" & ")" & """", 0, False