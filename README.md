# json-log-viewer


A console-based, interactive JSON log viewer.

Supports filters and limiting rendering to specific keys.

Filters are specified as (limited) JSON Paths + a predicate (Equals,
Has-Substring and Has-Key) + an operand (currently text only).

Currently depends on an unreleased change to vty-ui, so it can be built
normally yet. Will be fixed soon.


TODO:
- "pinning" (pinning a message should keep it on-screen even while scrolling
  through more messages, regardless of filters)
- regexes in filters
- support for piping in logs in the shell:
  `json-log-viewer 3< <(tail -F mylog.log | cut ...)`
