# This shell script runs makeContextEvent.py for all the TSV files
# Remember to use the appropriate virtualenv (NLP for Enrique)

TSVDIR="$1"
ANNDIR="$2"

echo "Processing $TSVDIR with $ANNDIR"

for F in "$TSVDIR"/*.tsv;
do
    #echo "$F"
    NAME=$(basename "$F")
    ID=${NAME%.tsv}

    # echo "$ANNDIR/$ID/event_context.txt"
    echo "Processing $ID..."
    python manualAnnotations2Intervals.py "$F"  # > "$ANNDIR/$ID/event_context.txt"
    mv manual_context_intervals.txt "$ANNDIR/$ID/"
    mv manual_event_intervals.txt "$ANNDIR/$ID/"
done
