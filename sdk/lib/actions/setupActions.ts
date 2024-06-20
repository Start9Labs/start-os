import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ExpectedExports } from "../types"
import { CreatedAction } from "./createAction"

export function setupActions<Manifest extends SDKManifest, Store>(
  ...createdActions: CreatedAction<Manifest, Store, any>[]
) {
  const myActions = async (options: { effects: Effects }) => {
    const actions: Record<string, CreatedAction<Manifest, Store, any>> = {}
    for (const action of createdActions) {
      actions[action.id] = action
    }
    return actions
  }
  const answer: {
    actions: ExpectedExports.actions
    actionsMetadata: ExpectedExports.actionsMetadata
  } = {
    actions(options: { effects: Effects }) {
      return myActions(options)
    },
    async actionsMetadata({ effects }: { effects: Effects }) {
      return Promise.all(
        createdActions.map((x) => x.ActionMetadata({ effects })),
      )
    },
  }
  return answer
}
