# unreleased <YYYY-MM-DD> <loc>

- Remove dependency on `repr`. Users of this library will now have to provide
  their own `equal` and `pp` functions for the values they are storing in
  their rtree (#45, @patricoferris).

# v0.2.0 2026-05-05 Belfast

- Add a set of `remove` functions (#44, @FayCarsons)

## Bugs, Fixes and Optimisations

- Fix `values` not returning entire tree (#40, @mdales)
- Added bounds function to get overall tree dimensions (#42, @mdales)

# v0.1.1 2023-08-17 Cambridge

## Bugs, Fixes and Optimisations

 - Remove extra length calculations (#16, @lindig)
 - Remove some polymorphic comparisons and replace with `Float` functions (#15, @patricoferris)
 - Fix stack overflows in OMT and Rectangle.merge (#14, @patricoferris, reported by @lindig)

# v0.1.0 2023-08-08 Munich

 - Initial release
