import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ExpectedExports } from "../types"
import { createUtils } from "../util"
import { once } from "../util/once"
import { Utils } from "../util/utils"
import { CreatedAction } from "./createAction"

export function setupActions<Manifest extends SDKManifest, Store>(
  ...createdActions: CreatedAction<Manifest, Store, any>[]
) {
  const myActions = async (options: {
    effects: Effects
    utils: Utils<Manifest, Store>
  }) => {
    const actions: Record<string, CreatedAction<Manifest, Store, any>> = {}
    for (const action of createdActions) {
      const actionMetadata = await action.metaData(options)
      actions[actionMetadata.id] = action
    }
    return actions
  }
  const answer: {
    actions: ExpectedExports.actions
    actionsMetadata: ExpectedExports.actionsMetadata
  } = {
    actions(options: { effects: Effects }) {
      const utils = createUtils<Manifest, Store>(options.effects)

      return myActions({
        ...options,
        utils,
      })
    },
    async actionsMetadata({ effects }: { effects: Effects }) {
      const utils = createUtils<Manifest, Store>(effects)
      return Promise.all(
        createdActions.map((x) => x.ActionMetadata({ effects, utils })),
      )
    },
  }
  return answer
}
