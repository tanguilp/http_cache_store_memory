# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

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
