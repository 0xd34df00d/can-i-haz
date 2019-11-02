# Changelog for can-i-haz

## 0.3.0.0

* Reexport `Control.Monad.Reader` from the `Control.Monad.Reader.Has` module with
  custom `ask`, `asks` and `reader` functions more compatible with the `Has` class.
* Similarly for `Control.Monad.Except`, `CoHas` and `throwError`/`liftError` functions.

## 0.2.1.0

* Added the `update` method to `Has` (yay lenses).
* Fixed the documentation regarding the recursiveness of the search.

## 0.2.0.1

* Less boilerplate for `Has` tuple instances and `CoHas` `Either` instance.
* Out-of-the-box `Has` support for up to sextuples due to the reduced boilerplate.

## 0.2.0.0

* Added `CoHas` class (dual to `Has`), allowing injecting values into sum types.

## 0.1.0.1

* Added documentation.
* Export the SearchSuccessful type which might aid hand-writing instances.

## 0.1.0.0

Initial release.
