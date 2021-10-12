Feature: Capture
  In order to update things
  As a user
  I want to materialize things

  Scenario: Materialization
    When I capture thing "Amsterdam" of class "city"
    And I materialize "Amsterdam" of class "city"
    Then I should be in the buffer "org-glance:<Amsterdam>"
