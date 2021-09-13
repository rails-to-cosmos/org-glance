@core
Feature: System Init
  In order to manage things
  As a user
  I want to define things, classes and domains

  Scenario: User configures org-glance
    Given `org-glance-directory' = view location
    And I change directory to a view location

  Scenario: User manually defines new category
    When I define view "Country"
    Then I should see message
      """
      View "Country" is now ready to glance
      """
    And I should have 1 view registered
    And I should have 0 headlines in view "Country"
