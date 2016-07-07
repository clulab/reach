ANNDIR="$1"

echo "Processing $ANNDIR"

for F in "$ANNDIR"/PMC*;
do
    #echo "$F"
    NAME=$(basename "$F")

    # echo "$ANNDIR/$ID/event_context.txt"
    echo "Processing $NAME..."
    sbt "runMain edu.arizona.sista.reach.context.ParsingExporter $F/sentences.txt $F/sections.txt $F/titles.txt"
    mv pos.txt deps.txt disc.txt disc.json $F
done
