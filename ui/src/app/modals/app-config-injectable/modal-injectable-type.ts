import { Type } from '@angular/core'
import { ValueType } from 'src/app/app-config/config-types'

export type AppConfigComponentMapping = { [k in ValueType]: Type<any> }
