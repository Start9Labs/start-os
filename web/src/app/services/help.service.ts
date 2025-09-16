import { Injectable, signal, TemplateRef } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class HelpService {
  readonly content = signal<TemplateRef<unknown> | null>(null)
}
