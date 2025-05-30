import { verify } from '@start9labs/argon2'
import {
  EMPTY,
  filter,
  MonoTypeOperatorFunction,
  of,
  pipe,
  switchMap,
  take,
} from 'rxjs'

export function verifyPassword(
  passwordHash: string | null,
  handler: (e: any) => void,
): MonoTypeOperatorFunction<string> {
  return pipe(
    filter(Boolean),
    switchMap(password => {
      try {
        verify(passwordHash || '', password)

        return of(password)
      } catch (e: any) {
        handler(e)

        return EMPTY
      }
    }),
    take(1),
  )
}
