Feature: Capture
  In order to update things
  As a user
  I want to materialize things

  Scenario: Basic materialization
    When I capture thing "Amsterdam" of class "city"
    And I materialize "Amsterdam" of class "city"
    Then I should be in the buffer "org-glance:<Amsterdam>"

  Scenario: Interactive materialization should reuse buffers
    When I capture thing "Munich" of class "city"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"
    And I kill buffer "org-glance:<Munich>"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"

  Scenario: Ability to materialize non-file buffers
    When I capture thing "Dmitry" of class "contact"
    And I materialize "Dmitry" of class "contact"
    Then I should be in the buffer "org-glance:<Dmitry>"
    And I materialize headline at point
    Then I should be in the buffer "org-glance:<Dmitry><2>"
