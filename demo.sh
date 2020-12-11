#!/usr/bin/env bash

# Flags="-sD -"
Flags=-sS

set -x

curl $Flags -H "content-type: application/json" http://localhost:8080/api/tables -X POST -d '{"name": "numbers"}'

curl $Flags -H "content-type: application/json" http://localhost:8080/api/tables/numbers -X POST -d '{"key": "one", "value": 1}'
curl $Flags -H "content-type: application/json" http://localhost:8080/api/tables/numbers -X POST -d '{"key": "two", "value": 2}'
curl $Flags -H "content-type: application/json" http://localhost:8080/api/tables/numbers -X POST -d '{"key": "three", "value": 3}'

curl $Flags http://localhost:8080/api/tables/numbers/one
curl $Flags http://localhost:8080/api/tables/numbers/two
curl $Flags http://localhost:8080/api/tables/numbers/three

curl $Flags http://localhost:8080/api/query/numbers/lookup?key=one
curl $Flags http://localhost:8080/api/query/numbers/lookup?key=two
curl $Flags http://localhost:8080/api/query/numbers/lookup?key=three

curl $Flags http://localhost:8080/api/query/numbers/lookup_by_date?date_from="2020-12-01T00:00:00Z"\&date_to="2099-01-01T23:59:59Z"
