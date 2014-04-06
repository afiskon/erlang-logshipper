erlang-logshipper
=================

Logshipper protocol implementation. Server side.

Work in progress. Not production ready.

```
$ ./rebar compile
$ erlc ./examples/server_example.erl
$ erl -pa ebin

1> server_example:start_link({127, 0, 0, 1}, 12345, 10).
```

Many thanks to @technocoreai who created Logshipper specification
and @kpy3 who agreed to release this library under open source license.
