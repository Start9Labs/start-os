import { Injectable, signal } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  readonly authenticated = signal(false)
}
