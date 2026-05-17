# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

## [1.0.1] - 2026-05-17

### Fixed

- [`http_cache_store_memory_worker`]: fix crash when invalidating by key a tuple

## [1.0.0] - 2026-05-16

No changes

## [0.3.3] - 2026-05-09

### Changed

- [`http_cache_store_memory`] Improve performance of invalidation by 1) making the
alternate keys a map instead of a list and 2) using `ets:select_delete/2` instead
of manual traversal

## [0.3.2] - 2024-11-25

### Changed

- Relax telemetry requirements

## [0.3.1] - 2023-07-05

### Fixed

- [`http_cache_store_memory_stats`] Fix an issue that occurs on systems that return only
free memory and system memory (via memsup)

## [0.3.0] - 2023-06-22

### Changed

- [`http_cache_store_memory`] Removed dependency to `http_cache`

## [0.2.0] - 2023-06-15

### Changed

- [`http_cache_store_memory_table_holder.erl`] ETS table holding cached HTTP responses is now
compressed for better performance
