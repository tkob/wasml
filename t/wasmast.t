# Setup

```
- use "wasmast.sml";
...
val it = () : unit
- open WasmAstEmptyPos;
...
- val span = ((), ());
...
```

# emit empty module

```
- emit print {exports=[], funcs=[], imports=[], memory=NONE, span=span, start=NONE, table=[], types=[]};
(module
)
val it = () : unit
```
