describe('Layout Overview', function() {
  it('presents a list of available town hall levels', function() {
    cy.visit('http://localhost:8000')

    cy.get('.town-hall-level select:first option')
      .should('have.length', 10)
      .each(($option, index, $select) => {
        var expected = '' + (11 - index + 1);
        if (index === 0 ) expected = 'Select Level';
        cy.wrap($option).should('have.value', expected);
      })
  })

  it('should start with no layout information', function() {
    cy.visit('http://localhost:8000')

    cy.get('.town-hall-level select:first option:first')
      .should('be.selected')
      .and('be.disabled')

    cy.get('.layout-title input:first')
      .should('have.value', '')
  })
})
