#!/bin/bash

################################################################
### path conversions

function get_new_path()
{
    echo "$1" | sed 's!/!--!g'
}

function get_old_path()
{
    echo "$1" | sed 's!--!/!g'
}

################################################################
### misc functions

function message()
{
    printf "$@\n"
} >&2


################################################################
### Destination directory check

DEST_DIR="$1"

if [ -z "$DEST_DIR" ]; then
    message "Usage: $0 DEST_DIR"
    exit 1
fi

if [ ! -d "$DEST_DIR" ]; then
    message "destination directory '$DEST_DIR' not found."
    exit 1
fi

################################################################
### cleanup function

TMPDIR=$(mktemp -d /tmp/convert-to-github-wiki.XXXXXXXX)
trap 'rm -fr $TMPDIR' ABRT EXIT HUP INT QUIT

OLD_FILE="$TMPDIR/old.$$"
NEW_FILE="$TMPDIR/new.$$"
RVT_FILE="$TMPDIR/revert.$$"

################################################################
### Pathname conversion (for example: convert '/' into '--')

for html_file in $(git-ls-files '*')
do
    old="$html_file"
    new=$(get_new_path "$html_file")
    rvt=$(get_old_path "$new")

    echo "$old" >> "$OLD_FILE"
    echo "$new" >> "$NEW_FILE"
    echo "$rvt" >> "$RVT_FILE"
done

################################################################
### Integrity check

################
## Revertable?

if diff -q "$OLD_FILE" "$RVT_FILE"; then
    message "Revertable? ...OK."
else
    message "Conversion failed (not revertable)."
    exit 1
fi

################
## No duplications?

if sort "$NEW_FILE" | uniq -c | grep -v '^  *1 '
then
    message "Conversion failed (some duplications)."
    exit 1
else
    message "No duplications? ...OK."
fi

################################################################
### Integrity check

paste "$OLD_FILE" "$NEW_FILE" |\
  awk "{printf \"./convert-link-text.rb '%s' > '$DEST_DIR/%s'\n\", \$1, \$2}" | sh

################################################################
### Invoke shell script

cd "$DEST_DIR"

for f in *.html
do
    md=`basename $f .html`.md

    # html2text-2.7 $f > "$md"
    # rm "$f"
    mv "$f" "$md"
done
