#!/bin/bash

# Fisierul sursa cu interogarile MongoDB
INPUT="interogariDisertatie.txt"

# Lista operatorilor MongoDB
OPERATORS=( "\$match" "\$group" "\$lookup" "\$project" "\$unwind" "\$facet" "\$merge" "\$out" "\$sort" "\$limit" "\$bucket" )

# Fisierul CSV 
OUTPUT="rezumat_interogari.csv"

# antet
echo "Operator,Nr_interogari" > "$OUTPUT"

# total
TOTAL=0

# Parcurgem operatorii + numaram
for op in "${OPERATORS[@]}"
do
    COUNT=$(grep -o "$op" "$INPUT" | wc -l)
    echo "${op},${COUNT}" >> "$OUTPUT"
    TOTAL=$((TOTAL + COUNT))
done

# linia de total
echo "TOTAL,${TOTAL}" >> "$OUTPUT"

echo "Rezumatul a fost salvat în '$OUTPUT'. In total avem $TOTAL interogari.)"
