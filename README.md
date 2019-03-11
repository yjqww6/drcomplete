# drcomplete

DrRacket Plugins for better autocompletion.

You can Install them all by
```
raco pkg install drcomplete
```
Or install ones independently(using subdirectory names).

## preview

### drcomplete-required

![gif](https://raw.githubusercontent.com/yjqww6/required-complete/gif/complete.gif)

### drcomplete-user-defined

![gif](https://raw.githubusercontent.com/yjqww6/user-defined-complete/gif/complete.gif)

### drcomplete-filename

![gif](https://raw.githubusercontent.com/yjqww6/filename-complete/gif/complete.gif)

### drcomplete-module

![default](https://user-images.githubusercontent.com/6269269/54006798-cc021100-4199-11e9-99bb-7fc25dd6d5d2.png)

## Notes
`drcomplete-require` completes identifiers required from other files and collections, identifiers required from same file are not supported currently.

`drcomplete-user-defined` completes identifiers introduced by user in binding positions in the same file.

Both `drcomplete-require` and `drcomplete-user-defined` need wait for background expansion to update autocompletion infomation.

For some constantly appearing patterns like `(let (... <cursor> ...) )` which fails to be expanded, `drcomplete-user-defined` also supports appending all user-written symbols(except the one before cursor) to candidates.

`drcomplete-filename` completes file paths, it use the directory of your file as current directory in definition window, and `(current-directory)` of your evaluation thread in repl.

`drcomplete-module` completes module paths for installed collections.

For `drcomplete-filename` and `drcomplete-module`, you would need to re-triger autocompletion after entering `/`(or `\` for file paths on Windows) to go inside subdirectories.
