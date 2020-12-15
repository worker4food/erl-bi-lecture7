[![Lecture7](https://github.com/worker4food/erl-bi-lecture7/workflows/Lecture7/badge.svg)](https://github.com/worker4food/erl-bi-lecture7/actions?query=workflow%3ALecture7)

## Usage
```
    $ rebar3 release
    $ _build/default/rel/cache/bin/cache daemon
    $ ./demo.sh
    $ _build/default/rel/cache/bin/cache stop
```

## Tests
```
    $ rebar3 ct
```

## Sample session
```
+ curl -sS -H 'content-type: application/json' http://localhost:8080/api/tables -X POST -d '{"name": "numbers"}'
{"result":"ok"}
+ curl -sS -H 'content-type: application/json' http://localhost:8080/api/tables/numbers -X POST -d '{"key": "one", "value": 1}'
{"result":"ok"}
+ curl -sS -H 'content-type: application/json' http://localhost:8080/api/tables/numbers -X POST -d '{"key": "two", "value": 2}'
{"result":"ok"}
+ curl -sS -H 'content-type: application/json' http://localhost:8080/api/tables/numbers -X POST -d '{"key": "three", "value": 3}'
{"result":"ok"}
+ curl -sS http://localhost:8080/api/tables/numbers/one
{"result":1}
+ curl -sS http://localhost:8080/api/tables/numbers/two
{"result":2}
+ curl -sS http://localhost:8080/api/tables/numbers/three
{"result":3}
+ curl -sS 'http://localhost:8080/api/query/numbers/lookup?key=one'
{"result":1}
+ curl -sS 'http://localhost:8080/api/query/numbers/lookup?key=two'
{"result":2}
+ curl -sS 'http://localhost:8080/api/query/numbers/lookup?key=three'
{"result":3}
+ curl -sS 'http://localhost:8080/api/query/numbers/lookup_by_date?date_from=2020-12-01T00:00:00Z&date_to=2099-01-01T23:59:59Z'
{"result":[2,3,1]}
```
