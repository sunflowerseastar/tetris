describe("basic app flow and use", () => {
  before(() => {
    cy.visit("/");
  });

  it("has a board and meta", () => {
    cy.get(".square").should("have.length", 200);
    cy.get(".upcoming-piece-square").should("have.length.gt", 3);
    cy.get(".rows-completed").contains("0");
    cy.get(".level").contains("1");
  });
});
