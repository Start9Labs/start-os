import { openAsBlob } from 'node:fs'
import { readFile } from 'node:fs/promises'
import { basename } from 'node:path'
import { createRepositoryApi } from '../lib/forge-api.mjs'

export async function publishRelease({ request, tag, name, body, files }) {
  const metadata = { tag_name: tag, name, body }
  let release = await request(
    'GET',
    `/releases/tags/${encodeURIComponent(tag)}`,
    { allowNotFound: true },
  )
  release = release
    ? await request('PATCH', `/releases/${release.id}`, { json: metadata })
    : await request('POST', '/releases', { json: metadata })

  for (const file of files) {
    const assetName = basename(file)
    const existing = release.assets?.find(asset => asset.name === assetName)
    if (existing)
      await request('DELETE', `/releases/${release.id}/assets/${existing.id}`)
    const form = new FormData()
    form.set('attachment', await openAsBlob(file), assetName)
    await request(
      'POST',
      `/releases/${release.id}/assets?name=${encodeURIComponent(assetName)}`,
      { body: form },
    )
  }
}

if (import.meta.main) {
  publishRelease({
    request: createRepositoryApi({
      apiUrl: `${process.env.GITHUB_SERVER_URL}/api/v1`,
      repository: process.env.GITHUB_REPOSITORY,
      token: process.env.FORGE_TOKEN,
    }),
    tag: process.env.RELEASE_TAG,
    name: process.env.RELEASE_NAME,
    body: await readFile(process.env.RELEASE_BODY_PATH, 'utf8'),
    files: (process.env.RELEASE_FILES || '').split(/\r?\n/).filter(Boolean),
  }).catch(error => {
    console.error(error)
    process.exitCode = 1
  })
}
