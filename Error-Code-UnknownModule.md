This error occurs when the type checker knows a token refers to a module, but is unable to find a definition for that module. For example,

```purs
import MyModule
```

yields

```
Unknown module MyModule
```

if there is no accompanying definition of `MyModule` in any loaded files.

# Possible Solution - Version 0.9.1

If in PSCI, a possible fix is to exit and restart PSCI with `pulp psci`.

If you start `psci` manually, you should make sure the file paths you pass into `psci` include the module you're trying to import, e.g.

```bash
# import all required bower .purs files
# import .purs files directly under src
psci 'bower_components/purescript-*/src/**/*.purs' 'src/*.purs'
```

# Possible Solution - Pre Version 0.9.1

If in PSCI, a possible fix is to load the file which has a definition for the module:

```purs
> :load src/MyProject.purs
> import MyModule
```

where `src/MyProject.purs` includes a definition for `MyModule`.
