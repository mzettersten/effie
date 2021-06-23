# set how long you want the fade-in/fade-out to last (in sec)
startTime = 0
language$ = "boskot_test_list.txt"
sounds$ = "audio"
saveTo$ = "final_audio_list"
saveAs$ = "boskot_test"

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
numberOfSoundsMinusOne = numberOfSounds - 1


# open all files and save durations, pitches, and intensities to table
for i to numberOfSoundsMinusOne
	select string
	file$ = Get string... i
	file'i'$ = file$
	file'i' = Read from file... 'sounds$'/'file$'
	Create Sound from formula: "silence_between", 1, 0, 'between$', 44100, "0"
endfor

#add final sound
select string
file$ = Get string... numberOfSounds
file'numberOfSounds'$ = file$
file'numberOfSounds' = Read from file... 'sounds$'/'file$'

Create Sound from formula: "silence_end", 1, 0, 0.0975, 44100, "0"

select all
minus Strings boskot_test_list
Concatenate
#endTime = Get end time
#Formula... if x<startTime then self * (1-(cos(0.5 * pi * (x/startTime)) ^ 2)) else self fi
#Formula... if (x > (endTime-startTime)) then self * (1-(cos((0.5 * pi * (( endTime - x)/startTime))) ^2)) else self fi

Write to WAV file... 'saveTo$'/'saveAs$'.wav

select all
Remove


