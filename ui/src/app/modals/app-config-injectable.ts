import { AppConfigObjectPage } from './app-config-object/app-config-object.page'
import { AppConfigListPage } from './app-config-list/app-config-list.page'
import { AppConfigUnionPage } from './app-config-union/app-config-union.page'
import { AppConfigValuePage } from './app-config-value/app-config-value.page'
import { Type } from '@angular/core'
import { ValueType } from 'src/app/pkg-config/config-types'

export const appConfigComponents: AppConfigComponentMapping = {
  'string': AppConfigValuePage,
  'number': AppConfigValuePage,
  'enum': AppConfigValuePage,
  'boolean': AppConfigValuePage,
  'list': AppConfigListPage,
  'object': AppConfigObjectPage,
  'union': AppConfigUnionPage,
  'pointer': undefined,
}

export type AppConfigComponentMapping = { [k in ValueType]: Type<any> }
