import { Injectable } from '@angular/core'
import { AbstractTuiValueTransformer, TuiDay, TuiTime } from '@taiga-ui/cdk'

type From = [TuiDay | null, TuiTime | null] | null
type To = string | null

@Injectable()
export class DatetimeTransformerService extends AbstractTuiValueTransformer<
  From,
  To
> {
  fromControlValue(controlValue: To): From {
    if (!controlValue) {
      return null
    }

    const day = TuiDay.jsonParse(controlValue.slice(0, 10))
    const time =
      controlValue.length === 16
        ? TuiTime.fromString(controlValue.slice(-5))
        : null

    return [day, time]
  }

  toControlValue(componentValue: From): To {
    if (!componentValue) {
      return null
    }

    const [day, time] = componentValue

    return day?.toJSON() + (time ? `T${time.toString()}` : '')
  }
}
