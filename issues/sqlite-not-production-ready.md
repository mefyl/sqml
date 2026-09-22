# SQLite should only be used for tests or local dev.

SQML tries to provide 100% coverage of PostgreSQL features with the SQLite
backend, but some workarounds make it unfit for production:

* [tag:SQLite has no sequences] so they are simulated via a `SELECT
  MAX(column) + 1`, which does the deed but is suboptimal.
