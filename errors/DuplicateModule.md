# `DuplicateModule` Error

## Cause

This error occurs when two modules have the same name.

## Fix

- Make sure your module names are correct.
- Check to make sure you do not have any duplicate modules in your dependencies.
- If you are passing input files to the compiler using a wildcard, make sure it is not
  including any files twice.

## Notes
