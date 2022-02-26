import { InjectionToken, Type } from '@angular/core'

export const LIST_HEADER_CONTENT = new InjectionToken<Type<any>>(
  'Dynamic component for the marketplace list header',
)
