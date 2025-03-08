#!/bin/bash

mi compile $1 --specific-test "$2" --output $3 && \
exec "$3"