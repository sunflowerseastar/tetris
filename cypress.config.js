// https://docs.cypress.io/guides/references/configuration
const { defineConfig } = require("cypress");

module.exports = defineConfig({
  screenshotOnRunFailure: false,
  video: false,
  e2e: {
    baseUrl: "http://localhost:9500",
    setupNodeEvents(on, config) {
      // implement node event listeners here
    },
  },
});
