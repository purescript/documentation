# `FileIOError`

## Cause

This error indicates that an I/O error occurred while compiling. This could be
caused, for example, by trying to write a compilation artifact to a directory
on a filesystem which has no space left on it, or by not having read or write
permissions on an output directory. The error message should usually contain
sufficient information to identify the specific issue.

## Fix

- If the error is due to having run out of space, try deleting or moving some
  files.
- If the error is due to permissions issues, check that you are running the
  compiler as the user you intended, and try changing the ownership of the
  output directory to the intended user.
