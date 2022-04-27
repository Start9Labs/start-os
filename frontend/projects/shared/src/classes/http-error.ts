import { HttpErrorResponse } from '@angular/common/http'

export class HttpError {
  readonly code = this.error.status
  readonly message = this.error.statusText
  readonly details = null
  readonly revision = null

  constructor(private readonly error: HttpErrorResponse) {}
}
