Here are the minutes from our meeting at LambdaConf 2015.

@michaelficarra @chexxor @andyarvanitis @benburdette

## Diamond Dependency Issues

- @chexxor presented the issue - see the [chexxor/purescript-dep-management](https://github.com/chexxor/purescript-dep-management) repository, which depends on two libraries: `mysquare` and `mycube`. Both of these libraries depend on the [chexxor/purescript-myexponent](https://github.com/chexxor/purescript-myexponent) library, but the former depends on v0.1 while the latter depends on v0.2.
- Bower gives a warning and requires the user to disambiguate the dependencies. In this case, the API has changed between the two versions (which is frustrating, as the library's major version didn't change, but should have), so the user's project will fail to compile on any choice the user makes.
- The decided solution is to have someone to update `mysquare` and `mycube` to compatible dependency versions. Given that we use Git for dependencies, sending a PR or forking a repo is the recommended approach, although it admittedly involves some work.
- This problem can be alleviated by prefixing dependency versions with `~`. This symbol allows Bower to resolve a version conflict, such as illustrated above, by choosing a version usable by all requesting libraries. This symbol indicates that any API-compatible version of that library can be used, where "API-compatible" is defined as version label having the same 1st number, presuming the version label is semver-conforming.

## Action items

### Documentation

- We should write a semver tutorial for beginners, specifically its use in PureScript and the way we rely on `~`-versions.
- Add package creator guidelines to the wiki.

### Verified Semver

- Bower does what we need but doesn't enforce compatible versions of packages, merely warns. We should develop either a package manager or a Bower plugin or separate tool for enforcing semver versioning using the subtyping relationship at the module level.

### Required Explicit Exports

- Even if all imported names were explicit, importing all names into the same qualified name for more than one module will still cause the same problem.

```purs
import qualified A as Q
import qualified B as Q
```

- Should create a tool to dump a module with all implicit imports replaced with their resolutions.

- Some pretty poor points were made in https://wiki.haskell.org/Import_modules_properly