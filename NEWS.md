# blueprint 0.0.0.9002

* **New concepts**
    - *strict atomic types*
        + Strict atomic are R types considered to be atomic objects in
        the package: `NULL`, `logical`, `integer`, `single` (yes, `single`, see
        documentation of the package for more information), `double`, `complex`,
        `character` and `raw` vectors.
        + These are the fundamental building blocks of R.
* **New classes**
    - `Atomic`
        + A class to hold blueprints of *strict atomic vectors*. This is an
        important class packed with a lot of features. Just like strict atomic
        vectors are the building blocks of R, so is class `Atomic` for the
        package. All other `blueprint`'s classes will reuse this class.
* **New features**
    - A new `%bp%` operator which can be used to construct blueprints in a
    concise way. We call it the *Blueprinter*.
    - New assertion functions: `is_single()`, `is_strict_atomic()`,
    `is_named_vctr()` and `is_named_list()`. These are very useful.
* **Important changes**
    - Significant changes to class `Blueprint` to ensure consistency with class
    `Atomic`.
    - Package now imports packages `jsonlite` and `yaml` for I/O.
* **Other important changes**
    - Hundreds of new unit tests. We now use a consistent test structure derived
    from `testthat` 3e edition.
    - New internal mechanisms / utility functions. They are more robust and
    better tested.
    - New options system for I/O with packages `jsonlite` and `yaml`.
    - New (experimental) Github workflows for future CI/CD pipeline.

# blueprint 0.0.0.9001

* **Package creation. Start tracking development process.**
* **New features**
    - Blueprint
        + A super-class that all other classes inherit.
        + Provides low-level mechanisms such as `$get()` and `$set()` methods.
