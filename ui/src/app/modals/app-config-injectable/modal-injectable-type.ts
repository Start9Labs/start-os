import { Type } from '@angular/core'
import { ValueType } from 'src/app/pkg-config/config-types'

export type AppConfigComponentMapping = { [k in ValueType]: Type<any> }
