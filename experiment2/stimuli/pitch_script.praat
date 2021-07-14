form Enter Full Path + /
sentence path ~/
endform

sep$ = ","
filedelete 'path$''name$'pitch_range.csv
header_row$ = "Filename" + sep$ + "f0_mean" + sep$ + "f0_sd"  + sep$ + "min_pitch" + sep$ + "max_pitch" + sep$ + "pitch_range" + newline$
header_row$ > 'path$'pitch_range.csv

Create Strings as file list...  list 'path$'*.wav
number_files = Get number of strings
for j from 1 to number_files
   select Strings list
   current_token$ = Get string... 'j'
   Read from file... 'path$''current_token$'
   To Pitch (ac)... 0.01 75 15 no 0.03 0.45 0.01 0.35 0.14 600 
	meanpitch = Get mean... 0 0 Hertz
	sdpitch = Get standard deviation... 0 0 Hertz
   minpitch = Get minimum... 0 0 Hertz Parabolic
   maxpitch = Get maximum... 0 0 Hertz Parabolic
   range = maxpitch - minpitch
   fileappend "'path$'pitch_range.csv" 'current_token$' 'sep$' 'meanpitch:4' 'sep$' 'sdpitch:4' 'sep$' 'minpitch:4' 'sep$' 'maxpitch:4' 'sep$' 'range:4' 'newline$'
   Remove
endfor