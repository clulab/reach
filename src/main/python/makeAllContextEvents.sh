# This shell script runs makeContextEvent.py for all the TSV files
# Remember to use the appropriate virtualenv (NLP for Enrique)

TSVDIR="$1"
ANNDIR="$2"

echo "Processing $TSVDIR with $ANNDIR"

DICT=$ANNDIR/states_keys.txt
LABELS=$ANNDIR/states_labels.txt

for F in "$TSVDIR"/*.tsv;
do
    #echo "$F"
    NAME=$(basename "$F")
    ID=${NAME%.tsv}

    # echo "$ANNDIR/$ID/event_context.txt"
    echo "Processing $ID..."
    python makeContextEvents.py "$F" "$DICT" "$LABELS" > "$ANNDIR/$ID/event_context.txt"
done
