/** @type {import('ts-jest').JestConfigWithTsJest} */
module.exports = {
  preset: "ts-jest",
  automock: false,
  testEnvironment: "node",
  rootDir: "./src/",
  modulePathIgnorePatterns: ["./dist/"],
}
