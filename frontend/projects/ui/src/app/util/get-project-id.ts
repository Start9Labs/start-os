import { ActivatedRoute } from '@angular/router'

export function getProjectId({ snapshot }: ActivatedRoute): string {
  const projectId = snapshot.paramMap.get('projectId')

  if (!projectId) {
    throw new Error('projectId is missing from route params')
  }

  return projectId
}
