Feature: Capture
  In order to update things
  As a user
  I want to materialize things

  Scenario: Materialization
    When I capture thing "Amsterdam" of class "city"
    And I materialize "Amsterdam" of class "city"
    Then I should be in the buffer "org-glance:<Amsterdam>"

  Scenario: Reuse material buffers
    When I capture thing "Munich" of class "city"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"
    And I kill buffer "org-glance:<Munich>"
    And I materialize "Munich" of class "city"
    Then I should be in the buffer "org-glance:<Munich>"
