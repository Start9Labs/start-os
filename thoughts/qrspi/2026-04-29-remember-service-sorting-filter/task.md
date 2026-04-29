# Task: Persist Service Sorting/Filter Preferences

The Services tab (dashboard) should remember the user's chosen sorting order and any filter settings across navigation within the same session, and ideally across browser sessions. Currently, the service table uses Taiga UI's `tuiTableSort` pipe with default name-based sorting, but these preferences are lost when the user navigates away and back. The marketplace already has a `FilterPackagesPipe` and `AbstractCategoryService` pattern for managing filter state — we need a similar approach for the installed services list.
