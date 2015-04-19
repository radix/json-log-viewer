# json-log-viewer

An interactive console-based JSON log viewer.

Supports filters on messages and customizing which keys to show.

Filters are specified as (limited) JSON Paths + a predicate (Equals,
Has-Substring and Has-Key) + an operand (currently text only).

Currently depends on an unreleased change to vty-ui, so it can't be built
normally yet. Will be fixed soon.

## usage

This will load a log file without streaming:
```
json-log-viewer 3< somelog.log
```


This will load up a log file and continue streaming new messages:
```
json-log-viewer 3< <(tail -n+0 -F somelog.log)
```

## TODO
- "pinning" (pinning a message should keep it on-screen even while scrolling
  through more messages, regardless of filters)
- regexes in filters
- animated gif(s) linked from this README to show off features
