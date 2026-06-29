import { Operation } from './json-patch-lib'

/**
 * An incremental state change. Contains the revision number and the
 * RFC 6902 patch operations needed to transition from the previous state.
 */
export type Revision = {
  /** Monotonically increasing revision number. */
  id: number
  /** The patch operations that produce this revision from the previous one. */
  patch: Operation<unknown>[]
}

/**
 * A complete snapshot of the database state at a given revision.
 *
 * @typeParam T - The shape of the stored document.
 */
export type Dump<T> = { id: number; value: T }

/**
 * A server message: either a full {@link Dump} (snapshot) or an incremental {@link Revision} (patch).
 *
 * @typeParam T - The shape of the stored document.
 */
export type Update<T> = Revision | Dump<T>

/**
 * The three JSON Patch operation types produced by patch-db.
 *
 * Only `add`, `remove`, and `replace` are used — `test`, `move`, and `copy` are not produced by the server.
 */
export enum PatchOp {
  ADD = 'add',
  REMOVE = 'remove',
  REPLACE = 'replace',
}
