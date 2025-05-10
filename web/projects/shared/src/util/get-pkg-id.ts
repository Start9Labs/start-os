import { inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'

export function getPkgId(): string {
  const pkgId = inject(ActivatedRoute).snapshot.paramMap.get('pkgId')

  if (!pkgId) {
    throw new Error('pkgId is missing from route params')
  }

  return pkgId
}
