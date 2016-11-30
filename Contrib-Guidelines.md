`purescript-contrib` is an organization for community-driven libraries. In an effort to avoid bitrotted code, the following guidelines will be enforced on a semi-regular basis:

- Projects should be **maintained**. Each library should have a clearly-identified maintainer who is responsible for responding to pull requests and issues. To yield maintainership, the maintainer should simply open an issue and we'll find a new maintainer or deprecate the library.
- Projects should be **up to date**. Specifically they should build against the current core library set (up to a reasonable time window), or be identified as deprecated or seeking a new maintainer.
- Projects should be **documented**. Typically, this just involves running `psc-docs` as part of the build process, but `contrib` libraries should also strive to document every exported type and function with some combination of text and examples.

Contrib libraries should try to follow the [Style Guide](Style-Guide) as closely as possible.

Maintainers who do not follow these guidelines will be asked to move their repositories out of `purescript-contrib`.
