# set how long you want the fade-in/fade-out to last (in sec)
startTime = 0
language$ = "L2_2.txt"
sounds$ = "audio"
saveTo$ = "final_audio_list"
saveAs$ = "L2_2"

# after you've entered the values above click run (or Command+R). the rest of the script below is automated :)

#silence to insert between elements
between$ = "0.5"

# remove open files
clearinfo
select all
nocheck Remove


# get files in the to read directory

#string = Read Strings: language$ 
string = Read Strings from raw text file: language$

#string = Read Strings from raw text file: language$
numberOfSounds = Get number of strings


# open all files and save durations, pitches, and intensities to table
for i to numberOfSounds
	select string
	file$ = Get string... i
	file'i'$ = file$
	file'i' = Read from file... 'sounds$'/'file$'
	Create Sound from formula: "silence_between", 1, 0, 'between$', 44100, "0"
endfor

select all
minus Strings L2_2
Concatenate
#endTime = Get end time
#Formula... if x<startTime then self * (1-(cos(0.5 * pi * (x/startTime)) ^ 2)) else self fi
#Formula... if (x > (endTime-startTime)) then self * (1-(cos((0.5 * pi * (( endTime - x)/startTime))) ^2)) else self fi

Write to WAV file... 'saveTo$'/'saveAs$'.wav

select all
Remove


