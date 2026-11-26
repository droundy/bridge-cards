The frozen card-location beliefs for the beliefs screen (branch join-beliefs):
the prior's and then the evidence's weights, one a line, as learn_card_locations
wrote them (run p01: 2000 rounds of 128 deals of td-f003-na's round-108000
learner 4, prior from walls at 10, Adam 0.01). Unzip and pass the path as
PLAY_SELF_LOCATIONS. Held-out log loss 5.675 nats a play against 7.032 for
vacant places. Not for merging.

weights-d01.txt.gz (2026-11-25): the same recipe as p01, refitted with the declarer's plays from the dummy as
evidence for the defenders (join-beliefs 72f3525 and later; the evidence tables have three players, and older
files are read with the dummy's entries 0). Held-out nats a play after every card, from the opening lead on:
defenders 5.389 against p01's 5.410, declarer 6.547 against 6.546. Used by lat3-s91, hom-s81/s82 and the corpus
screens of the beliefs tables.

weights-d02.txt.gz (2026-11-26): d01's recipe refitted on games whose opening leads keep the lead conventions
(lead-conventions, merged in join-beliefs 3fc8155), with the lead's facts taken by the beliefs as a count on the
rank block above the card led (join-beliefs d21d53f) and the lead's fitted evidence skipped. Held-out nats a play
on new-rule deals: declarer 6.444, defenders 5.227 (d01 on the same deals: 6.465 and 5.258; d02 without the lead
facts: 6.517 and 5.266).
