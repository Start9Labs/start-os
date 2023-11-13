import { HttpErrorResponse } from '@angular/common/http'

export class HttpError {
  constructor(private readonly error: HttpErrorResponse) {}

  readonly code = this.error.status
  readonly message = this.error.statusText
}
