This error occurs when the type checker knows a token refers to a type, but is unable to find a definition for that type. For example,

```purs
f :: Foo
```

yields

```
Error at  line 1, column 1 - line 1, column 6:
  Unknown type Foo
```

without an accompanying definition of `Foo` either in the module or imported.