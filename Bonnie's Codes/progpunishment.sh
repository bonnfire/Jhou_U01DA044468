## Make executable: chmod +x script-name-here.sh

(cd ~/Dropbox\ (Palmer\ Lab)/Palmer\ Lab/Bonnie\ Lin/Tom_Jhou_U01DA044468_Dropbox_copy/Progressive\ punishment/U6 && exec ~/Dropbox\ (Palmer\ Lab)/Palmer\ Lab/Bonnie\ Lin/github/Jhou_U01DA044468/Bonnie\'s\ Codes/progpunishment.sh) # run a program with diff working directory from current

# for each file, get 
tac '2018-0719-1748_6_FOOD CONFLICT.txt' | awk '{$3 ~ /TIMEOUT$/?f++:f=0} {if(f==3) print NR -1} '

find -type f -iname "*CONFLICT*.txt" -print0 | xargs -0  awk '{$3 ~ /TIMEOUT$/?f++:f=0} {if(f==3)print NR "," $2 "," FILENAME}' # works for U6, gets the line number of the third TIMEOUT

Session #	
# Avg	NA 
# Date	FILENAME
# Time	FILENAME
# FR	NA
# Weight (%) NA	
# Last block completed (mA)	MA VALUE FOR THE LINE PRECEDING THREE TIMEOUTS ---OR---
# Last trial attempted (mA)	MA VALUE FOR THE "THIS TRIAL" LINE PRECEDING PREVIOUS "THIS TRIAL" LINE
# of trials at last Shock Intensity	
# Active (left)	
# Inactive (right)	
# Box #	
# notes	EXTRACT FROM EXCEL

# (not runnable workspace) find -type f -iname "*CONFLICT*.txt" -print0 | xargs -0 tac | awk '{$3 ~ /TIMEOUT$/?f++:f=0} {if(f==3) print NR -1 "," FILENAME}'

# Friday 10/18 progpunishment.sh: line 3: syntax error near unexpected token `('

