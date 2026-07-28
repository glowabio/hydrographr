#!/bin/bash
set -e

SOURCE=/home/bootstrap-data
TARGET=/home/participant/data_write

# Only populate if volume is empty
if [ -z "$(ls -A $TARGET)" ]; then
    echo "Initializing volume with default data..."
    cp -r $SOURCE/* $TARGET/
fi

exec "$@"
