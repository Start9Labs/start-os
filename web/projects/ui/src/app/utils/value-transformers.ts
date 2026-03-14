import { TuiDay, TuiTime, TuiValueTransformer } from '@taiga-ui/cdk'

type From = [TuiDay, TuiTime | null] | null
type To = string | null

export const DatetimeTransformer: TuiValueTransformer<From, To> = {
  fromControlValue: controlValue => {
    if (!controlValue) {
      return null
    }

    const day = TuiDay.jsonParse(controlValue.slice(0, 10))
    const time =
      controlValue.length === 16
        ? TuiTime.fromString(controlValue.slice(-5))
        : null

    return [day, time]
  },

  toControlValue: componentValue => {
    if (!componentValue) {
      return null
    }

    const [day, time] = componentValue

    return day?.toJSON() + (time ? `T${time.toString()}` : '')
  },
}

export const DateTransformer: TuiValueTransformer<
  TuiDay | null,
  string | null
> = {
  fromControlValue: value => (value ? TuiDay.jsonParse(value) : null),
  toControlValue: value => value?.toJSON() || null,
}
