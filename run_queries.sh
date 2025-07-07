#!/bin/bash

# MongoDB connection details
MONGO_URI="mongodb://localhost:26061"
DB_NAME="tpch10__schema_431"

# Parameters
QUERY_FILE=$1
SF=$2
NODES=$3
RUN_WITHOUT_EXPLAIN=$4

DATE_TIME=$(date "+%Y%m%d-%H%M%S")
OUTPUT_DIR="explain_results_sf${SF}_${NODES}n_${DATE_TIME}"
mkdir -p "$OUTPUT_DIR"

# Check if query file is provided
if [ -z "$QUERY_FILE" ]; then
    echo "Usage: $0 <query_file> <sf> <nodes> [run_without_explain]"
    exit 1
fi

# Metadata files
QUERY_METADATA_FILE="${OUTPUT_DIR}/query_metadata_with_explain.csv"
QUERY_METADATA_FILE_WITHOUT_EXPLAIN="${OUTPUT_DIR}/query_metadata_without_explain.csv"

# Create header only if file doesn't exist
if [ ! -f "$QUERY_METADATA_FILE" ]; then
    echo "query_number,query_text,query_start_time,query_end_time,execution_time_ms" > "$QUERY_METADATA_FILE"
fi

if [[ "$RUN_WITHOUT_EXPLAIN" == "true" ]] && [ ! -f "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN" ]; then
    echo "query_number,query_text,query_start_time,query_end_time,execution_time_ms" > "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN"
fi

# Logging helper
log() {
    echo "$(date "+%Y-%m-%d %H:%M:%S.%3N") $1"
}

# Query execution functions
run_query_explain() {
    local query="$1"
    local line_number="$2"
    local output_file="${OUTPUT_DIR}/explain_output_${line_number}.json"

    local explained_query="${query}.explain(\"executionStats\")"
    echo "$explained_query" | timeout 600 mongosh "$MONGO_URI/$DB_NAME" --quiet > "$output_file"

    # If error or timeout, write fallback JSON
    if [ $? -ne 0 ] || [ ! -s "$output_file" ]; then
        echo '{"stages":[]}' > "$output_file"
        log "⚠️ Query $line_number failed or timed out"
    fi
}

run_query_without_explain() {
    local query="$1"
    local line_number="$2"
    local output_file="${OUTPUT_DIR}/results_without_explain_${line_number}.json"

    echo "$query" | timeout 580 mongosh "$MONGO_URI/$DB_NAME" --quiet > "$output_file"

    if [ $? -ne 0 ] || [ ! -s "$output_file" ]; then
        echo "timeout" > "$output_file"
        log "⚠️ Query $line_number (without explain) failed or timed out"
    fi
}

# Track already processed queries for resume capability
processed_lines=()
if [ -f "$QUERY_METADATA_FILE" ]; then
    processed_lines=($(tail -n +2 "$QUERY_METADATA_FILE" | cut -d, -f1))
fi
declare -A processed_map
for num in "${processed_lines[@]}"; do
    processed_map[$num]=1
done

processed_lines_wo=()
if [[ "$RUN_WITHOUT_EXPLAIN" == "true" ]] && [ -f "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN" ]; then
    processed_lines_wo=($(tail -n +2 "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN" | cut -d, -f1))
    declare -A processed_map_wo
    for num in "${processed_lines_wo[@]}"; do
        processed_map_wo[$num]=1
    done
fi

# Read and process queries
line_number=0
total_queries=$(wc -l < "$QUERY_FILE")

while IFS= read -r line || [ -n "$line" ]; do
    ((line_number++))

    # Skip already processed (with explain)
    if [[ ${processed_map[$line_number]+_} ]]; then
        continue
    fi

    log "Starting query $line_number out of $total_queries"

    query_start_time=$(date "+%Y-%m-%d %H:%M:%S.%3N")
    start_ns=$(date +%s%N)

    if [[ "$RUN_WITHOUT_EXPLAIN" != "true" ]]; then
        run_query_explain "$line" $line_number
    fi

    query_end_time=$(date "+%Y-%m-%d %H:%M:%S.%3N")
    end_ns=$(date +%s%N)
    execution_time_ms=$(( (end_ns - start_ns) / 1000000 ))

    escaped_query=$(echo "$line" | sed 's/"/""/g')
    printf "%s,\"%s\",%s,%s,%s\n" "$line_number" "$escaped_query" "$query_start_time" "$query_end_time" "$execution_time_ms" >> "$QUERY_METADATA_FILE"
    sync

    if [[ "$RUN_WITHOUT_EXPLAIN" == "true" ]]; then
        # Skip if already run without explain
        if [[ ${processed_map_wo[$line_number]+_} ]]; then
            continue
        fi

        query_start_time_wo=$(date "+%Y-%m-%d %H:%M:%S.%3N")
        start_ns_wo=$(date +%s%N)

        run_query_without_explain "$line" $line_number

        query_end_time_wo=$(date "+%Y-%m-%d %H:%M:%S.%3N")
        end_ns_wo=$(date +%s%N)
        execution_time_ms_wo=$(( (end_ns_wo - start_ns_wo) / 1000000 ))

        printf "%s,\"%s\",%s,%s,%s\n" "$line_number" "$escaped_query" "$query_start_time_wo" "$query_end_time_wo" "$execution_time_ms_wo" >> "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN"
        sync
    fi

    log "Ending query $line_number"
done < "$QUERY_FILE"

# Move and zip results
mv "$QUERY_METADATA_FILE" "$OUTPUT_DIR/"
[[ "$RUN_WITHOUT_EXPLAIN" == "true" ]] && mv "$QUERY_METADATA_FILE_WITHOUT_EXPLAIN" "$OUTPUT_DIR/"

zip -r "${OUTPUT_DIR}.zip" "$OUTPUT_DIR"
log "✅ All done! Results saved in $OUTPUT_DIR and zipped."
