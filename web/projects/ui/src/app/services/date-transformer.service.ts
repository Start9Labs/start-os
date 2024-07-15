import { Injectable } from '@angular/core'
import { TuiDay, TuiValueTransformer } from '@taiga-ui/cdk'

type From = TuiDay | null
type To = string | null

@Injectable()
export class DateTransformerService extends TuiValueTransformer<From, To> {
  fromControlValue(controlValue: To): From {
    return controlValue ? TuiDay.jsonParse(controlValue) : null
  }

  toControlValue(componentValue: From): To {
    return componentValue?.toJSON() || null
  }
}
