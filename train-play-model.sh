#!/bin/bash
# Trains the play model further by self-play and publishes the result.
#
# It takes the play model file that robot/play-model.txt names (fetching it if
# need be; see fetch-play-model.sh), plays ROUNDS (500) more rounds of
# self-play from it on THREADS (every core) threads, as a population of four
# learners in a round robin of mixed partnerships on fresh deals (a file with
# fewer learners gets new ones that start from nothing), then plays the new
# best model against the old one in a two-table match of 2000 deals.  If the new model is
# no worse there, or FORCE=1, it uploads the new play model file to the GitHub
# release play-models and points robot/play-model.txt at it, for you to
# commit.  Uploading publishes the file: the repository is public.
#
# LEARNERS (4) sets the size of the population, and SHAPES (v2) the shapes of
# the learners that the file lacks, comma-separated and taken in turn (see
# Shape in robot/src/ai/fitted_play.rs).  With FROM_SCRATCH=1 every learner
# starts from nothing, so that shapes can be compared on the same training.
# Every learner is also scored against the robot and against the old model,
# which do not learn, at each snapshot.

set -eo pipefail
cd "$(dirname "$0")"
rounds=${ROUNDS:-500}
threads=${THREADS:-$(nproc)}
dir=target/train-play-model
mkdir -p "$dir"

./fetch-play-model.sh --exact
old="$dir/old.bin"
cp robot/src/ai/play_model.bin "$old"

# A file left from an earlier run must not be taken for this one's.
rm -f "$dir/new.bin"
resume=()
if [ "$FROM_SCRATCH" != 1 ]; then
    resume=(PLAY_SELF_RESUME="$PWD/$old")
fi
echo "Training for $rounds rounds on $threads threads."
env "${resume[@]}" PLAY_SELF_CHECKPOINT="$PWD/$dir/new.bin" PLAY_SELF_REFERENCE="$PWD/$old" \
    PLAY_SELF_ROUNDS="$rounds" PLAY_THREADS="$threads" PLAY_SELF_REPORT=20 \
    PLAY_SELF_LEARNERS="${LEARNERS:-4}" PLAY_SELF_SHAPES="${SHAPES:-v2}" \
    PLAY_SELF_ROUND_ROBIN=1 PLAY_SELF_WILDCARDS=4 \
    PLAY_SELF_DEALS=128 PLAY_SELF_POOL=128 PLAY_SELF_FRESH=128 \
    PLAY_SELF_ENTROPY=0.25 PLAY_SELF_DECAY=0.01 \
    PLAY_SELF_PAST=0.2 PLAY_SELF_RANDOM=0.05 PLAY_SELF_SNAPSHOT=100 PLAY_SELF_TOURNAMENT=200 \
    PLAY_SELF_WATCH="$PWD/$dir/watch.html" \
    cargo test -p robot --release self_play -- --ignored --exact --nocapture ai::fitted_play::tests::self_play \
    | grep --line-buffered -E "^round|^resuming|^learners|panicked"
if [ ! -s "$dir/new.bin" ]; then
    echo "Training wrote no model file." >&2
    exit 1
fi

echo "Playing the new model against the old one."
result=$(PLAY_MATCH_WEIGHTS="$PWD/$dir/new.bin" PLAY_MATCH_AGAINST="$PWD/$old" PLAY_MATCH_DEALS=2000 \
    PLAY_THREADS="$threads" \
    cargo test -p robot --release match_against_the_robot -- --ignored --exact --nocapture \
    ai::fitted_play::tests::match_against_the_robot | grep "deals in")
echo "$result"
gain=$(echo "$result" | sed -nE 's/.*took ([-+]?[0-9.]+) tricks.*/\1/p')
if [ -z "$gain" ]; then
    echo "Could not read the gain from the match." >&2
    exit 1
fi
if awk -v gain="$gain" 'BEGIN { exit !(gain + 0 < 0) }' && [ "$FORCE" != 1 ]; then
    echo "The new model is worse than the old one, so it is not published (FORCE=1 publishes it anyway)."
    exit 1
fi

# The round is the first number after the 8 bytes of the magic (see
# robot/src/ai/fitted_play/model_file.rs).
round=$(od -An -tu4 -j8 -N4 "$dir/new.bin" | tr -d ' ')
name=$(printf 'play_model-round-%06d.bin' "$round")
cp "$dir/new.bin" "$dir/$name"
gh release upload play-models "$dir/$name"
sum=$(sha256sum "$dir/$name" | cut -d' ' -f1)
cat > robot/play-model.txt <<MANIFEST
# The play model file that the robot's build includes, kept as an asset of the
# GitHub release play-models rather than in git: fetch-play-model.sh fetches it
# and checks it against this SHA-256, and train-play-model.sh replaces it.
url https://github.com/droundy/bridge-cards/releases/download/play-models/$name
sha256 $sum
MANIFEST
cp "$dir/$name" robot/src/ai/play_model.bin
echo "Published $name; commit robot/play-model.txt to use it."
