@core
Feature: System Init
  In order to manage things
  As a user
  I want to define things, classes and domains

  @view
  Scenario: Define class
    When I define view "Country"
    Then I should see message
      """
      View "Country" is now ready to glance
      """
    # And I should have 1 view registered
    # And I should have 0 headlines in overview "Country"
