module DrumSongs exposing
    ( initialTextVoice1
    , initialTextVoice2
    , sample1TextVoice1
    , sample1TextVoice2
    , sample2TextVoice1
    , sample2TextVoice2
    )


initialTextVoice1 =
    """This app turns text into a kind of techno-music by imitating
the princples of African drum languages.  Things to try: (1) put text
in this box and press "Play".  (2) Alter the tempo (beats per minute).
(3) Try patterns, e.g., "Wawachaachaadadadada,,"  Here spaces and
commas both give one beat rests. (4) It is fun to experiment with
patterns like palindromes: What is si tahW is si sii sii,,,,
repetition, etc.
"""


initialTextVoice2 =
    """Note that there are two voices.  The first (above) is playing
in quarter notes, while the second (here) is playing in eighth notes.

To my ear, Sample 2 is the most interesting.  Note that the two parts
have lengths 36 and 25, in quarter and eighth notes, respectively. Counted
in eighth notes, the lengths are 72 and 25.  Thus the song repeats itself
every 72 x 25 = 1800 eighth notes, or what is the same, every 900 quarter notes.
(In general, the song length in eighth notes is the least common multiple of
twice the length of part 1 and the length of part 2.)
"""


sample1TextVoice1 =
    "What is that,,,!!"


sample1TextVoice2 =
    ",,,,,,zzaa ww at ta azz"



--sample1TextVoice1 =
--    "What is si tahW is si sii sii,,,,"


sample2TextVoice1 =
    "MississipiipississiM,,,,,,,,,,,,,,,,"



--sample1TextVoice2 =
--    ",,,,,,zzaawwattaazz"
--,,,,,!!,,l,l,l,l,,,kkkk
--b,b,b,b,,,,aakkllaa,,,kkll,,,,,,,
{-
   missi! missi! pipi!! missi!!,,,,,,,,,
   ,,,,,,,,!!zz !!aa !! ww !! at ta ! azz !

-}


sample2TextVoice2 =
    "Wawacha,,,,VxxLaadxxVdaaL"



--   "Wawachaachaadadadada,,,,"
