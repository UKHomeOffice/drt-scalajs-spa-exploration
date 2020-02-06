// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This is will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

let todayAtString = require ('./time-helpers').todayAtUtcString

Cypress.Commands.add('setRoles', (roles = []) => {
  return cy.request("POST", '/test/mock-roles', { "roles": roles});
});

const portRole = ["test"]
const lhrPortRole = ["LHR"]
const bfRoles = ["border-force-staff", "forecast:view", "fixed-points:view", "arrivals-and-splits:view", "desks-and-queues:view"];
const bfPlanningRoles = ["staff:edit"];
const superUserRoles = ["create-alerts", "manage-users"];
const portOperatorRoles = ["port-operator-staff", "arrivals-and-splits:view", "api:view-port-arrivals"]

Cypress.Commands.add('asABorderForceOfficer', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": portRole.concat(bfRoles)});
});

Cypress.Commands.add('asATestPortUser', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": portRole});
});

Cypress.Commands.add('asAnLHRPortUser', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": lhrPortRole});
});

Cypress.Commands.add('asANonTestPortUser', () => {
  return cy.request("POST", '/test/mock-roles', { "roles":[]});
});

Cypress.Commands.add('asABorderForcePlanningOfficer', () => {
  const roles = portRole.concat(bfRoles).concat(bfPlanningRoles);
  return cy.request("POST", '/test/mock-roles', { "roles": roles});
});

Cypress.Commands.add('asADrtSuperUser', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": superUserRoles.concat(bfRoles).concat(portRole)});
});

Cypress.Commands.add('asATestSetupUser', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": superUserRoles.concat(bfRoles).concat(portRole)});
});

Cypress.Commands.add('asAPortOperator', () => {
  return cy.request("POST", '/test/mock-roles', { "roles": portOperatorRoles.concat(portRole)});
});

Cypress.Commands.add('asABorderForceOfficerWithRoles', (roles = []) => {
  const withRoles = roles.concat(bfRoles).concat(portRole);
  return cy.request("POST", '/test/mock-roles', { "roles": withRoles})
});

Cypress.Commands.add('addFlight', (params) => {
  let defaults = {
    "Operator": "TestAir",
    "Status": "On Chox",
    "EstDT": todayAtString(12, 0),
    "ActDT": todayAtString(12, 0),
    "EstChoxDT": todayAtString(12, 0),
    "ActChoxDT": todayAtString(12, 0),
    "Gate": "46",
    "Stand": "44R",
    "MaxPax": 78,
    "ActPax": 51,
    "TranPax": 0,
    "RunwayID": "05L",
    "FlightID" : 100,
    "BaggageReclaimId": "05",
    "AirportID": "MAN",
    "Terminal": "T1",
    "ICAO": "TS123",
    "IATA": "TS123",
    "Origin": "AMS",
    "SchDT": todayAtString(12, 0)
  };

  const flightPayload = Object.assign({}, defaults, params);

  cy.request('POST', '/test/arrival', flightPayload);
});

Cypress.Commands.add('deleteData', () => cy.request("DELETE", '/test/data'));

Cypress.Commands.add('saveShifts', (shiftsJson) => cy.request("POST", "/data/staff", shiftsJson));

Cypress.Commands.add('navigateHome', () => cy.visit('/'));

Cypress.Commands.add('navigateToMenuItem', (itemName) => cy
  .get('.navbar-drt li')
  .contains(itemName)
  .click(5, 5, { force: true })
);

Cypress.Commands.add('selectCurrentTab', () => cy.get('#currentTab').click())

Cypress.Commands.add('findAndClick', (toFind) => cy.contains(toFind).click({ force: true }));

Cypress.Commands.add('choose24Hours', () => cy.get('#current .date-selector .date-view-picker-container')
.contains('24 hours')
.click());

Cypress.Commands.add('chooseArrivalsTab', () => cy.get("#arrivalsTab").click());

Cypress.Commands.add('addManifest', (manifest) => cy.request('POST', '/test/manifest', manifest));

Cypress.Commands.add('waitForFlightToAppear', (flightCode) => {
  return cy
    .navigateHome()
    .navigateToMenuItem('T1')
    .selectCurrentTab()
    .get("#currentTab").click()
    .get("#arrivalsTab").click()
    .choose24Hours()
    .get("#arrivals")
    .contains(flightCode);
})
