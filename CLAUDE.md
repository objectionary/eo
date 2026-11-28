# Instructions for Claude

## Tests for EO objects

Never write Java tests for EO objects.
An EO object, such as `eo-runtime/src/main/eo/directory.eo`,
  keeps all its tests inside its own `.eo` file,
  as test attributes that start with `++>`.
If a behavior of an EO object needs a test,
  add a `++>` test attribute to that object,
  even when the behavior depends on the operating system.
Java test classes, such as `EOdirectoryEOtmpfileTest`,
  must never target EO objects.
Java tests are only for Java code,
  such as atoms, the runtime, and the Maven plugin.

## Lint skips in eo-runtime

Never skip a lint in `eo-runtime/pom.xml`.
The POM must have no `<skipSourceLints>` and no `<skipProgramLints>`,
  not even for a single lint or for a short time.
When a lint complains about an EO object in `eo-runtime`,
  fix the EO code.
When the complaint is a false positive,
  report it to `objectionary/lints` for source lints,
  or to `objectionary/wpa` for program lints,
  and wait for a release with the fix.
