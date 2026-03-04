import { ActionRes } from 'src/app/services/api/api.types'

type ActionResponse = NonNullable<ActionRes>
type ActionResult = NonNullable<ActionResponse['result']>
export type ActionResponseWithResult = ActionResponse & { result: ActionResult }
export type SingleResult = ActionResult & { type: 'single' }
export type GroupResult = ActionResult & { type: 'group' }
