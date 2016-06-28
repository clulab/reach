# This shell script runs makeHtml.py for all the TSV files
# Remember to use the appropriate virtualenv (NLP for Enrique)

ANNDIR="$1"

echo "Processing $ANNDIR"

for F in "$ANNDIR"/PMC*;
do
    #echo "$F"
    NAME=$(basename "$F")

    # echo "$ANNDIR/$ID/event_context.txt"
    echo "Processing $NAME..."
    python makeHtml.py "$F"
done
