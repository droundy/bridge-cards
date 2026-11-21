#!/bin/bash
# Puts the weights of the play model that robot/play-model.txt names in
# robot/src/ai/play_model.bin, where the robot's build includes them.  They are
# not kept in git but as an asset of the GitHub release play-models, and are
# checked against the manifest's SHA-256.
#
# Weights already there that differ from the manifest's, such as ones just
# trained, are kept with a warning, unless this is run with --exact, as
# deploy.sh does, so that what is deployed is what the manifest names.

set -e
cd "$(dirname "$0")"
manifest=robot/play-model.txt
file=robot/src/ai/play_model.bin
if [ ! -f "$manifest" ]; then
    echo "No play model is named yet ($manifest), so the robot is built without one." >&2
    exit 0
fi
url=$(awk '$1 == "url" { print $2 }' "$manifest")
sum=$(awk '$1 == "sha256" { print $2 }' "$manifest")

if [ -f "$file" ]; then
    if echo "$sum  $file" | sha256sum --check --status; then
        exit 0
    fi
    if [ "$1" != "--exact" ]; then
        echo "Keeping $file, which is not the play model that $manifest names." >&2
        exit 0
    fi
fi

partial="$file.download"
curl --fail --location --silent --show-error --output "$partial" "$url"
if ! echo "$sum  $partial" | sha256sum --check --status; then
    rm -f "$partial"
    echo "The play model at $url is not the one that $manifest names." >&2
    exit 1
fi
mv "$partial" "$file"
echo "Fetched the play model from $url."
