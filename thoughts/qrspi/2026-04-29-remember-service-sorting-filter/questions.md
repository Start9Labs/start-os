# Research Questions

## Context
The codebase is an Angular 21 + TypeScript web UI for the StartOS dashboard, using Taiga UI components. The Services dashboard (`dashboard.component.ts`) displays installed services in a table (`table.component.ts`) with Taiga's `tuiTableSort` pipe. The marketplace has a `FilterPackagesPipe` and `AbstractCategoryService` for category/query filtering. Storage is handled via `StorageService` (wrapping `WA_LOCAL_STORAGE`) and `ClientStorageService` (which uses `StorageService` for reactive key-value persistence).

## Questions
1. How does Taiga UI's `TuiTable` / `tuiTableSort` pipe handle sorting state, and is there a built-in mechanism to persist or control the active sort column and direction?
2. What patterns exist in the codebase for persisting user preferences to Patchdb instead `localStorage` — specifically how `StorageService` and `ClientStorageService` are used, and what key naming conventions are followed?
3. How does the `FilterPackagesPipe` in the marketplace project work, and how does `AbstractCategoryService` manage filter state (query string, category) as reactive streams?
4. What is the full shape of `PackageDataEntry` / `StateInfo` in `DataModel`, and what fields are available for filtering (e.g., service state/status, version, health)?
5. How does the `PatchDB` service provide reactive data streams, and what is the lifecycle of the `watch$('packageData')` observable used in the dashboard?
