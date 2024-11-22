export async function stringFromStdErrOut(x: {
  stdout: string
  stderr: string
}) {
  return x?.stderr ? Promise.reject(x.stderr) : x.stdout
}
