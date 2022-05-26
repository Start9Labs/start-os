import { ActivatedRoute } from '@angular/router'

export function getPkgId({ snapshot }: ActivatedRoute): string {
  const pkgId = snapshot.paramMap.get('pkgId')

  if (!pkgId) {
    throw new Error('pkgId is missing from route params')
  }

  return pkgId
}
